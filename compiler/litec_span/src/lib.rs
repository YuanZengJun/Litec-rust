#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    start: usize,
    end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
    
    pub fn start(&self) -> usize {
        self.start
    }

    pub fn end(&self) -> usize {
        self.end
    }
    
    pub fn len(&self) -> usize {
        self.end - self.start
    }
    
    pub fn is_empty(&self) -> bool {
        self.start == self.end
    }
    
    pub fn extend_to(&self, other: Span) -> Self {
        let start = self.start.min(other.start);
        let end = self.end.max(other.end);
        Self::new(start, end)
    }
    
    pub fn contains(&self, pos: usize) -> bool {
        self.start <= pos && pos < self.end
    }
    
    pub fn overlaps(&self, other: Span) -> bool {
        self.start < other.end && other.start < self.end
    }
    
    pub fn merge(&self, other: Span) -> Option<Self> {
        if self.overlaps(other) || self.end == other.start || other.end == self.start {
            Some(self.extend_to(other))
        } else {
            None
        }
    }
    
    pub fn extract<'a>(&self, source: &'a str) -> Option<&'a str> {
        if self.end <= source.len() {
            Some(&source[self.start..self.end])
        } else {
            None
        }
    }
}

#[derive(Debug)]
pub struct SourceFile<'src> {
    pub name: &'src str,
    pub source: &'src str,
}

impl<'src> SourceFile<'src> {
    pub fn new(name: &'src str, source: &'src str) -> Self {
        Self {
            name: name,
            source: source
        }
    }
}

#[derive(Debug)]
pub struct SourceMap<'a> {
    sources: Vec<SourceFile<'a>>
}

impl<'a> SourceMap<'a> {
    pub fn new() -> Self {
        Self {
            sources: Vec::new()
        }
    }

    pub fn add_file(&mut self, source_file: SourceFile<'a>) -> usize {
        let len = self.sources.len();
        self.sources.push(source_file);
        len
    }

    pub fn get_file(&self, id: usize) -> Option<&SourceFile<'a>> {
        self.sources.get(id)
    }
}

#[cfg(test)]
mod tests;