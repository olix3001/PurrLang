use std::{env::current_dir, fs, io::{BufWriter, Write}, path::{Path, PathBuf}};

use codegen::blocks::Sb3Code;
use colored::Colorize;
use common::PurrSource;
use compiler::compile_purr;
use error::{create_error_report, ErrorReport, SyntaxError};
use once_cell::sync::Lazy;
use parser::{ast::NodeId, parser::parse_purr};
use resolution::project_tree::ProjectTree;
use zip::{write::SimpleFileOptions, ZipWriter};

use crate::{cache::PurrCache, project::{Costume, Project}, sb3::{ScratchCostume, ScratchProject, Target}};

static ZIP_OPTIONS: Lazy<SimpleFileOptions> = Lazy::new(
   || SimpleFileOptions::default().compression_method(zip::CompressionMethod::Stored)
);

#[derive(clap::Args)]
pub struct BuildCommand {
    directory: Option<PathBuf>,
    #[arg(long, short)]
    target: Option<PathBuf>
}

pub fn build_project(args: BuildCommand) {
    let path = args.directory.clone().unwrap_or(current_dir().unwrap());
    let target = args.target.clone().unwrap_or(path.join("out.sb3"));

    let target_writer = fs::File::create(&target).unwrap();
    let mut zip = ZipWriter::new(BufWriter::new(target_writer));

    println!(
        "  {} {}",
        "Compiling".bold().bright_green(),
        path.canonicalize().unwrap().to_str().unwrap()
    );

    let Ok(project) = fs::read_to_string(path.join("Catnip.toml"))
    else {
        eprintln!(
            "{}: {}",
            "Error".bold().bright_red(),
            "Could not find Catnip.toml in given project directory.".bold()
        );
        return;
    };
    let project: Project = match toml::from_str(&project) {
        Ok(project) => project,
        Err(err) => {
            eprintln!(
                "{}: {}",
                "Error".bold().bright_red(),
                "Catnip.toml is not a valid project file.".bold()
            );
            eprintln!("{}", err.to_string());
            return;
        }
    };

    build_(&args, &path.as_path(), &project, &mut zip);
    zip.finish().unwrap();

    println!(
        "  {} {}",
        "Compiled as".bold().bright_green(),
        target.canonicalize().unwrap().to_str().unwrap()
    );
}

fn build_(
    args: &BuildCommand,
    project_dir: &Path,
    project: &Project,
    zip: &mut ZipWriter<BufWriter<fs::File>>
) {
    let mut scratch = ScratchProject::default();

    // Enable extensions
    scratch.extensions = project.project.extensions.clone();

    // Compile sprites
    for (sprite_name, sprite) in project.sprites.iter() {
        let is_stage = sprite_name == "Stage";
        let mut scratch_sprite = Target {
            is_stage: if is_stage { true } else { false },
            name: sprite_name.clone(),
            x: sprite.position.x,
            y: sprite.position.y,
            volume: sprite.volume,
            size: sprite.size,
            direction: sprite.direction,
            visible: sprite.visible,
            rotation_style: sprite.rotation_style.clone(),
            layer_order: if is_stage { 0 } else { sprite.layer },
            draggable: sprite.draggable,

            tempo: if is_stage { Some(60) } else { None },
            video_transparency: if is_stage { Some(50) } else { None },
            video_state: if is_stage { Some("on".to_string()) } else { None },
            text_to_speech_language: if is_stage { Some(serde_json::Value::Null) } else { None },
            ..Default::default()
        };

        if sprite.costumes.len() == 0 {
            eprintln!(
                "{}: {} {} {}",
                "Error".bold().bright_red(),
                "Sprite".bold(),
                sprite_name.bold().bright_yellow(),
                "requires at least one costume but got 0.".bold()
            );
            return;
        }
        for costume in sprite.costumes.iter() {
            if let Some(costume) = project_costume_to_scratch_costume(
                project_dir,
                costume,
                zip
            ) {
                scratch_sprite.costumes.push(costume);
            } else { return; }
        }

        let Some(current_costume_id) = scratch_sprite.costumes.iter()
            .position(|costume| costume.name == sprite.costume)
        else {
            eprintln!(
                "{}: {} {} {} {}{}",
                "Error".bold().bright_red(),
                "Costume".bold(),
                sprite.costume.bold().bright_yellow(),
                "does not exist on sprite".bold(),
                sprite_name.bold().bright_yellow(),
                ".".bold()
            );
            return;
        };
        scratch_sprite.current_costume = current_costume_id as _;

        if let Some(entry) = &sprite.entry {
            if let Some(code) = build_file(
                entry.as_path(),
                project_dir
            ) {
                scratch_sprite.blocks = code.blocks;
                scratch_sprite.variables = code.variables;
            } else { return; }
        }

        if is_stage {
            scratch.targets.insert(0, scratch_sprite);
        } else {
            scratch.targets.push(scratch_sprite);
        }
    }

    // Build project.json
    zip.start_file("project.json", ZIP_OPTIONS.clone()).unwrap();
    let project_json = serde_json::to_vec(&scratch).unwrap();
    zip.write_all(&project_json).unwrap();
}

fn project_costume_to_scratch_costume(
    project_dir: &Path,
    costume: &Costume,
    zip: &mut ZipWriter<BufWriter<fs::File>>
) -> Option<ScratchCostume> {
    let src_path = project_dir.join("assets").join(&costume.src);
    if !src_path.exists() {
        eprintln!(
            "{}: {} {} {}",
            "Error".bold().bright_red(),
            "Asset".bold(),
            costume.src.to_str().unwrap().bold().bright_yellow(),
            "does not exist.".bold()
        );
        return None;
    }
    let file = fs::read(&src_path).unwrap();
    let hash = md5::compute(&file);
    let hash = hex::encode(&hash.0[..]);

    let file_name = format!("{}.{}", hash, costume.src.extension().unwrap().to_str().unwrap());

    zip.start_file(file_name.clone(), ZIP_OPTIONS.clone()).unwrap();
    zip.write_all(&file).unwrap();

    let scratch_costume = ScratchCostume {
        name: costume.name.clone(),
        bitmap_resolution: 1,
        data_format: costume.src.extension().unwrap().to_str().unwrap().to_string(),
        rotation_center_x: 0,
        rotation_center_y: 0,
        asset_id: hash,
        md5ext: file_name
    };

    Some(scratch_costume)
}

fn build_file(
    entry: &Path,
    project_dir: &Path
) -> Option<Sb3Code> {
    let src_path = project_dir.join("src").join(entry);
    if !src_path.exists() {
        eprintln!(
            "{}: {} {} {}",
            "Error".bold().bright_red(),
            "Source file".bold(),
            entry.to_str().unwrap().bold().bright_yellow(),
            "does not exist in src/.".bold()
        );
        return None;
    }

    let src = fs::read_to_string(&src_path).unwrap();

    let mut ast = match parse_purr(src, PurrSource::File(src_path.to_path_buf())) {
        Ok(ast) => ast,
        Err(err) => {
            let cache = PurrCache::default();
            create_error_report(ErrorReport::from(err)).eprint(cache).unwrap();
            return None;
        }
    };

    match inline_file_modules(&src_path, &mut ast.0) {
        Ok(_) => {},
        Err(err) => {
            let cache = PurrCache::default();
            create_error_report(ErrorReport::from(err)).eprint(cache).unwrap();
            return None;
        }
    }

    // TODO: Clean modules that are not imported

    let project_tree = ProjectTree::build_from_ast(Default::default(), &ast.0);
    let resolved = match resolution::resolve::resolve(&ast, &project_tree) {
        Ok(resolved) => resolved,
        Err(err) => {
            let cache = PurrCache::default();
            create_error_report(ErrorReport::from(err)).eprint(cache).unwrap();
            return None;
        }
    };

    let result = match compile_purr(
        &ast.0,
        PurrSource::File(src_path.to_path_buf()),
        &resolved
    ) {
        Ok(result) => result,
        Err(err) => {
            let cache = PurrCache::default();
            create_error_report(ErrorReport::from(err)).eprint(cache).unwrap();
            return None;
        }
    };

    Some(result)
}

fn inline_file_modules(
    entry: &Path,
    ast: &mut Vec<parser::ast::Item>
) -> Result<(), SyntaxError> {
    use parser::ast;
    let mut dir = fs::read_dir(entry.parent().unwrap()).unwrap();
    while let Some(Ok(file)) = dir.next() {
        let path = file.path();
        if path == entry { continue; }
        if path.is_file() {
            let mod_name = path.file_stem().unwrap().to_str().unwrap().to_string();
            if mod_name == "mod" { continue; }
            let src = fs::read_to_string(&path).unwrap();
            let source = PurrSource::File(path.clone());
            let mod_ast = parse_purr(src, source.clone())?;
            ast.push(ast::Item {
                kind: ast::ItemKind::Module(ast::ModuleDefinition {
                    name: mod_name,
                    body: mod_ast.0,
                    source
                }) ,
                id: NodeId::next(),
                pos: 0..0,
                attributes: mod_ast.1.attributes
            });
        } else {
            if path.join("mod.purr").exists() {
                let mod_path = path.join("mod.purr");
                let mod_name = path.file_name().unwrap().to_str().unwrap().to_string();
                let src = fs::read_to_string(&mod_path).unwrap();
                let source = PurrSource::File(mod_path.clone());
                let mod_ast = parse_purr(src, source.clone())?;
                let mut definition = ast::ModuleDefinition {
                    name: mod_name,
                    body: mod_ast.0,
                    source
                };

                inline_file_modules(&mod_path, &mut definition.body)?;

                ast.push(ast::Item {
                    kind: ast::ItemKind::Module(definition) ,
                    id: NodeId::next(),
                    pos: 0..0,
                    attributes: mod_ast.1.attributes
                });
            }
        }
    }
    Ok(())
}
