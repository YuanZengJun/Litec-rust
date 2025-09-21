use std::{
    collections::HashMap,
    sync::{Arc, RwLock},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StringId(pub usize);

impl std::fmt::Display for StringId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", get_global_string(*self).unwrap())
    }
}

#[derive(Debug, Default)]
struct StringPoolInner {
    strings: Vec<Arc<str>>,
    index_map: HashMap<Arc<str>, StringId>,
}

#[derive(Debug, Default)]
pub struct StringPool {
    inner: RwLock<StringPoolInner>,
}

impl StringPool {
    /// 创建新的空字符串池
    pub fn new() -> Self {
        Self {
            inner: RwLock::new(StringPoolInner::default()),
        }
    }
    
    /// 添加字符串到池中，返回其ID
    /// 如果字符串已存在，返回现有ID
    pub fn intern(&self, s: &str) -> StringId {
        let mut inner = self.inner.write().unwrap();
        
        // 检查是否已存在
        if let Some(&id) = inner.index_map.get(s) {
            return id;
        }
        
        // 创建新条目
        let id = StringId(inner.strings.len());
        let arc_str: Arc<str> = Arc::from(s);
        inner.index_map.insert(arc_str.clone(), id);
        inner.strings.push(arc_str);
        id
    }
    
    /// 根据ID获取字符串的只读引用
    pub fn get(&self, id: StringId) -> Option<Arc<str>> {
        let inner = self.inner.read().unwrap();
        inner.strings.get(id.0).cloned()
    }
    
    /// 检查字符串是否已在池中
    pub fn contains(&self, s: &str) -> bool {
        let inner = self.inner.read().unwrap();
        inner.index_map.contains_key(s)
    }
    
    /// 获取池中字符串数量
    pub fn len(&self) -> usize {
        let inner = self.inner.read().unwrap();
        inner.strings.len()
    }
    
    /// 检查池是否为空
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn to_string(&self) -> String {
        let inner = self.inner.read().unwrap();
        format!("{:#?}", inner)
    }
}

// 全局字符串池
pub static GLOBAL_STRING_POOL: once_cell::sync::Lazy<StringPool> =
    once_cell::sync::Lazy::new(StringPool::new);

/// 全局字符串插入函数
pub fn intern_global(s: &str) -> StringId {
    GLOBAL_STRING_POOL.intern(s)
}

/// 全局字符串查询函数
pub fn get_global_string(id: StringId) -> Option<Arc<str>> {
    GLOBAL_STRING_POOL.get(id)
}

/// 检查全局字符串池是否包含某字符串
pub fn contains_global(s: &str) -> bool {
    GLOBAL_STRING_POOL.contains(s)
}

/// 获取全局字符串池的大小
pub fn global_pool_len() -> usize {
    GLOBAL_STRING_POOL.len()
}

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