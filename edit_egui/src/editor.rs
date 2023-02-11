use std::{io::Result, path::Path, process::Command};

pub trait Editor {
    /// Goto symbol in path at line:column.
    fn goto(&self, symbol: &str, path: &Path, location: (usize, usize)) -> Result<()>;

    /// Insert new symbol at path with template. Template is language-specific.
    fn insert(&self, symbol: &str, path: &Path, template: &str) -> Result<()>;
}

#[derive(Default)]
pub struct EmacsClient;

impl Editor for EmacsClient {
    fn goto(&self, _symbol: &str, path: &Path, (line, column): (usize, usize)) -> Result<()> {
        let _output = Command::new("emacsclient")
            .arg("-n")
            .arg(format!("+{}:{}", line, column))
            .arg(format!("{}", path.display()))
            .output()?;
        Ok(())
    }

    fn insert(&self, _symbol: &str, path: &Path, template: &str) -> Result<()> {
        let _output = Command::new("emacsclient")
            .arg("-n")
            .arg("-e")
            .arg(format!(
                r#"(with-current-buffer (find-file "{}") (goto-char (point-max)) (insert "{}"))"#,
                path.display(),
                template
            ))
            .output()?;
        Ok(())
    }
}
