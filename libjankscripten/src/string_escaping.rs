use thiserror::Error;

#[derive(Debug, Error)]
#[error("syntax error in a string at position {offset}: {message}. Original string is {original}")]
pub struct UnescapingError {
    pub original: String,
    pub offset: usize,
    pub message: String
}

fn err(original: &str, offset: usize, message: &str) -> UnescapingError {
    return UnescapingError {
        original: original.to_string(),
        offset: offset,
        message: message.to_string()
    };
}

/// Turns a string that holds a JavaScript string literal into the string value that it represents.
///
/// The provided string must include the opening and closing quote.
///
/// TODO(arjun): Someone needs to read the ECMAScript specification to confirm
/// that the code here is legit.
// I had written a version of this function when we were using Ressa as the JavaScript parser. We
// deleted this function after adopting SWC. It turns out we need to deal with escaping in NotWasm
// too, and we have adopted JavaScript's escaping rules.
pub fn unescape_string(s: &str) -> Result<String, UnescapingError> {
    let mut buf = String::with_capacity(s.len());
    let mut iter = s.chars().peekable();
    let mut offset = 0;

    match iter.next() {
        None => return Err(err(s, offset, "no characters in string")),
        Some('"') | Some('\'') => { }
        Some(_) => return Err(err(s, offset, "string does not begin with \" or \'"))
    }

    while let Some(ch) = iter.next() {
        offset += 1;
        if ch == '"' || ch == '\'' {
            // TODO(arjun): Allow unescaped ' in a double-quoted string, and vice versa.
            // TODO(arjun): Require terminal quote to be the same as the initial quote.
            if iter.peek().is_none() {
                return Ok(buf);
            }
            return Err(err(s, offset, "extra characters after the quote"));
        }
        if ch != '\\' {
            buf.push(ch);
            continue;
        }
        let char_after_backslash = iter.next()
        .ok_or(err(s, offset, "character after backslash"))?;
        offset += 1;
        match char_after_backslash
        {

            '\'' | '"' | '\\' => buf.push(char_after_backslash),
            'n' => buf.push('\n'),
            'r' => buf.push('\r'),
            't' => buf.push('\t'),
            'f' => buf.push('\x0C'),
            'b' => buf.push('\x08'),
            'v' => buf.push('\x0B'),
            'x' => {
                let hex_s = format!(
                    "{}{}",
                    iter.next()
                        .ok_or(err(s, offset, "first hex digit after \\x"))?,
                    iter.next()
                        .ok_or(err(s, offset, "second hex digit after \\x"))?,
                );
                let n = u8::from_str_radix(&hex_s, 16).map_err(|_| {
                    err(s, offset, &format!("invalid escape \\x{}", &s))
                })?;
                buf.push(n as char);
            }
            'u' => {
                let hex_s = format!(
                    "{}{}{}{}",
                    iter.next()
                        .ok_or(err(s, offset, "first hex digit after \\x"))?,
                    iter.next()
                        .ok_or(err(s, offset, "second hex digit after \\x"))?,
                    iter.next()
                        .ok_or(err(s, offset, "third hex digit after \\x"))?,
                    iter.next()
                        .ok_or(err(s, offset, "fourth hex digit after \\x"))?
                );
                let n = u16::from_str_radix(&hex_s, 16).map_err(|_| {
                    err(s, offset, &format!("invalid unicode escape {}", &s))
                })?;
                buf.push(std::char::from_u32(n as u32).ok_or(err(s, offset, 
                    &format!("invalid Unicode character {}", n)
                ))?);
            }
            ch => {
                if ch < '0' || ch > '9' {
                    // JavaScript allows you to escape any character. If it's not a valid escape
                    // sequence, you just get the character itself. For example, '\😂' === '😂'.
                    buf.push(ch);
                } else {
                    let mut octal_str = String::with_capacity(2);
                    // First octal digit
                    octal_str.push(ch);
                    // Potentially octal digit
                    match iter.peek() {
                        Some(ch) if *ch >= '0' && *ch <= '7' => {
                            octal_str.push(*ch);
                            iter.next(); // consume
                        }
                        _ => (),
                    }
                    let n = u32::from_str_radix(&octal_str, 8).map_err(|_| {
                        err(s, offset, 
                            &format!("invalid octal escape \\u{}", &octal_str)
                        )
                    })?;
                    // 2-digit octal value is in range for UTF-8, thus unwrap should succeed
                    buf.push(std::char::from_u32(n).unwrap());
                }
            }
        }
    }

    return Err(err(s, offset, "missing closing quote"));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn no_escapes_double_quote() {
        assert_eq!(unescape_string(r#""hello, world""#).unwrap(), "hello, world");
    }

    #[test]
    fn no_escapes_single_quote() {
        assert_eq!(unescape_string(r#"'hello, world'"#).unwrap(), "hello, world");
    }

    #[test]
    fn escaped_quotes_in_double_quote() {
        assert_eq!(unescape_string(r#""\"\'""#).unwrap(), r#""'"#);
    }
}