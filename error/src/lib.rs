use common::{FileRange, PurrSource};
use info::{CodeArea, ErrorInfo};

pub mod info;

#[derive(Debug)]
pub struct ErrorReport {
    pub info: ErrorInfo,
    pub message: String,
    pub labels: Vec<(CodeArea, String)>,
    pub note: Option<String>
}

#[derive(Debug)]
pub enum SyntaxError {
    ExpectedToken {
        expected: String,
        found: String,
        pos: FileRange,
        file: PurrSource
    },
    UnexpectedToken {
        found: String,
        pos: FileRange,
        file: PurrSource
    },
    SyntaxError {
        message: String,
        pos: FileRange,
        file: PurrSource
    },
    Custom(ErrorReport)
}

impl From<SyntaxError> for ErrorReport {
    fn from(err: SyntaxError) -> Self {
        match err {
            SyntaxError::ExpectedToken { expected, found, pos, file } =>
                create_error(
                    ErrorInfo::from_area(CodeArea { pos: pos.clone(), file: file.clone() }),
                    "Syntax error",
                    &[(
                        CodeArea { pos, file },
                        &format!(
                            "Expected '{}' found '{}'.",
                            expected, found
                        )
                    )],
                    None
                ),
            SyntaxError::UnexpectedToken { found, pos, file } =>
                create_error(
                    ErrorInfo::from_area(CodeArea { pos: pos.clone(), file: file.clone() }),
                    "Syntax error",
                    &[(
                        CodeArea { pos, file },
                        &format!("Unexpected '{}'.", found)
                    )],
                    None
                ),
            SyntaxError::SyntaxError { message, pos, file } =>
                create_error(
                    ErrorInfo::from_area(CodeArea { pos: pos.clone(), file: file.clone() }),
                    "Syntax error",
                    &[(
                        CodeArea { pos, file },
                        &message
                    )],
                    None
                ),
            SyntaxError::Custom(report) => report
        }
    }
}

pub fn create_error<S: AsRef<str>>(
    info: ErrorInfo,
    message: S,
    labels: &[(CodeArea, S)],
    note: Option<S>
) -> ErrorReport {
    ErrorReport {
        info,
        message: message.as_ref().to_string(),
        labels: labels.iter()
            .map(|(area, msg)| match &area.file {
                PurrSource::File(file) => {
                    if !file.exists() {
                        (
                            CodeArea {
                                pos: 0..0,
                                file: PurrSource::Unknown
                            },
                            msg.as_ref().to_string()
                        )
                    } else {
                        (area.clone(), msg.as_ref().to_string())
                    }
                },
                _ => (area.clone(), msg.as_ref().to_string())
            }).collect(),
        note: note.map(|note| note.as_ref().to_string())
    }
}

pub fn create_error_report(rep: ErrorReport) -> ariadne::Report<'static, CodeArea> {
    use ariadne::{Config, Label, Report, ReportKind};

    let mut report = Report::build(
        ReportKind::Error,
        rep.info.position.file,
        rep.info.position.pos.start
    )
    .with_config(Config::default().with_cross_gap(true))
    .with_message(rep.message);

    for (i, (area, label)) in rep.labels.iter().enumerate() {
        report = report.with_label(
            Label::new(area.clone())
                .with_message(&format!("{}: {}", i.to_string(), label))
                .with_order(i as _)
                .with_priority(2)
        )
    }

    if let Some(note) = rep.note {
        report = report.with_note(note);
    }

    report.finish()
}
