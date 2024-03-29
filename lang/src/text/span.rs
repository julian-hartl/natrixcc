use fusion_compiler::bug;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TextSpan {
    pub start: usize,
    pub end: usize,
    pub literal: String,
}

impl TextSpan {
    pub fn new(start: usize, end: usize, literal: String) -> Self {
        Self {
            start,
            end,
            literal,
        }
    }

    pub fn combine(mut spans: Vec<TextSpan>) -> TextSpan {
        if spans.is_empty() {
            bug!("Cannot combine empty spans")
        }
        spans.sort_by(|a, b| a.start.cmp(&b.start));

        let start = spans.first().unwrap().start;
        let end = spans.last().unwrap().end;

        TextSpan::new(
            start,
            end,
            spans.into_iter().map(|span| span.literal).collect(),
        )
    }

    pub fn length(&self) -> usize {
        self.end - self.start
    }

    pub fn literal<'a>(&self, input: &'a str) -> &'a str {
        &input[self.start..self.end]
    }
}
