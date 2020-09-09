type CharLocation = (usize, usize);

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn location() {
        let location = Location::new(String::from("a cool filename"), (0, 3), (1, 6));
        assert_eq!(location.file(), String::from("a cool filename"));
        assert_eq!(location.start(), (0, 3));
        assert_eq!(location.end(), (1, 6));
        let location = Location::new(String::new(), (0, 0), (0, 0));
        assert_eq!(location.file(), "");
        assert_eq!(location.start(), (0, 0));
        assert_eq!(location.end(), (0, 0));
    }
    #[test]
    #[should_panic]
    fn wrong_location() {
        let location = Location::new(String::from("some file"), (1, 0), (0, 0));
    }
    #[test]
    #[should_panic]
    fn wrong_location2() {
        let location = Location::new(String::from("some file"), (1, 5), (1, 3));
    }
}

#[derive(Debug, PartialEq)]
pub struct Location {
    file: String,
    start: CharLocation,
    end: CharLocation,
}

impl Location {
    pub fn new(file: String, start: CharLocation, end: CharLocation) -> Self {
        assert!(start.0 < end.0 || (start.0 == end.0 && start.1 <= end.1));
        Self { file, start, end }
    }

    pub fn from_stream_pos(file: String, stream: &str, start_pos: usize, end_pos: usize) -> Self {
        let mut current_char = 0;
        let mut current_line = 0;
        let mut current_pos = 0;
        assert!(start_pos <= end_pos);
        let start;
        let mut chr;
        loop {
            chr = stream.chars().nth(current_pos);
            if current_pos == start_pos || chr.is_none() {
                start = (current_line, current_char);
                break;
            }
            if chr == Some('\n') {
                current_line += 1;
                current_char = 0;
            } else {
                current_char += 1;
            }
            current_pos += 1;
        }
        let end;
        loop {
            chr = stream.chars().nth(current_pos);
            if current_pos == end_pos || chr.is_none() {
                end = (current_line, current_char);
                break;
            }

            if chr == Some('\n') {
                current_line += 1;
                current_char = 0;
            } else {
                current_char += 1;
            }
            current_pos += 1;
        }

        Self { file, start, end }
    }

    pub fn file(&self) -> &str {
        &self.file[..]
    }

    pub fn start(&self) -> CharLocation {
        self.start
    }

    pub fn end(&self) -> CharLocation {
        self.end
    }
}
