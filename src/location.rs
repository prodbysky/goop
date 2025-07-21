#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Span {
    begin: usize,
    end: usize,
}

impl Span {
    pub fn len(&self) -> usize {
        assert!(self.begin <= self.end);
        self.end - self.begin
    }

    pub fn begin(&self) -> usize {
        self.begin
    }

    pub fn end(&self) -> usize {
        self.end
    }

    pub fn new(begin: usize, end: usize) -> Self {
        assert!(begin <= end);
        Self { begin, end }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Spanned<T> {
    pub s: Span,
    pub v: T,
}

impl<T> Spanned<T> {
    pub fn new(v: T, s: Span) -> Self {
        Self { v, s }
    }

    pub fn inner(&self) -> &T {
        &self.v
    }

    pub fn to_inner(self) -> T {
        self.v
    }

    pub fn span(&self) -> &Span {
        &self.s
    }

    pub fn begin(&self) -> usize {
        self.span().begin
    }

    pub fn end(&self) -> usize {
        self.span().end
    }

    pub fn map<U>(self, f: fn(Spanned<T>) -> U) -> Spanned<U> {
        Spanned {
            s: self.s,
            v: f(self),
        }
    }
}
