use crate::BitSet;
use std::{iter, ops::Range};

pub struct Graph {
    ranges: Vec<u32>,
    edges: Vec<u32>,
}

impl Graph {
    pub fn new() -> Self {
        Self {
            ranges: Vec::new(),
            edges: Vec::new(),
        }
    }

    pub fn add_node(&mut self, nodes: impl IntoIterator<Item = u32>) {
        self.edges.extend(nodes);
        self.ranges.push(self.edges.len() as u32);
    }

    pub fn children_range(&self, node: u32) -> Range<u32> {
        let start = node.checked_sub(1).map_or(0, |i| self.ranges[i as usize]);
        let end = self.ranges[node as usize];
        start..end
    }

    pub fn len(&self) -> usize {
        self.ranges.len()
    }

    pub fn clear(&mut self) {
        self.ranges.clear();
        self.edges.clear();
    }
}

pub struct CycleDetector {
    stack: Vec<Range<u32>>,
    order: Vec<u32>,
    seen: BitSet,
    in_stack: BitSet,
}

impl Default for CycleDetector {
    fn default() -> Self {
        Self {
            stack: Vec::new(),
            order: Vec::new(),
            seen: BitSet::new(),
            in_stack: BitSet::new(),
        }
    }
}

impl CycleDetector {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn detect(&mut self, root: u32, graph: &Graph) -> Option<Vec<u32>> {
        self.stack.clear();
        self.seen.clear();
        self.in_stack.clear();
        self.seen.resize(graph.len());
        self.in_stack.resize(graph.len());

        self.in_stack.insert(root as usize);
        self.stack.push(graph.children_range(root));

        while let Some(range) = self.stack.last_mut() {
            let Some(current) = range.next() else {
                self.stack.pop();
                if let Some(current) = self.stack.last() {
                    let current = graph.edges[current.start as usize - 1];
                    self.in_stack.remove(current as usize);
                    self.seen.insert(current as usize);
                    self.order.push(current);
                }
                continue;
            };

            let current = graph.edges[current as usize];

            if self.seen.contains(current as usize) {
                continue;
            }

            if !self.in_stack.insert(current as usize) {
                let cycle = self
                    .stack
                    .iter()
                    .rev()
                    .skip(1)
                    .map(|r| graph.edges[r.start as usize - 1])
                    .take_while(|r| *r != current)
                    .chain(iter::once(current))
                    .collect();
                return Some(cycle);
            }

            self.stack.push(graph.children_range(current));
        }

        self.order.push(root);

        None
    }

    pub fn order(&self) -> &[u32] {
        &self.order
    }
}

#[cfg(test)]
mod test {
    use super::{CycleDetector, Graph};

    #[test]
    fn no_cycles() {
        let mut detector = CycleDetector::new();

        let mut graph = Graph::new();
        graph.add_node([1, 2, 3]);
        graph.add_node([2, 3]);
        graph.add_node([3]);
        graph.add_node([]);

        assert!(detector.detect(0, &graph).is_none());

        let mut graph = Graph::new();
        graph.add_node([1, 2, 3]);
        graph.add_node([3]);
        graph.add_node([3, 1]);
        graph.add_node([]);

        assert!(detector.detect(0, &graph).is_none());
    }

    #[test]
    fn has_cycles() {
        let mut detector = CycleDetector::new();

        let graph = {
            let mut graph = Graph::new();
            graph.add_node([1, 2, 3]);
            graph.add_node([2, 3]);
            graph.add_node([3]);
            graph.add_node([0]);
            graph
        };

        assert_eq!(detector.detect(0, &graph), Some(vec![3, 2, 1, 0]));

        let graph2 = {
            let mut graph = Graph::new();
            graph.add_node([1, 2, 3]);
            graph.add_node([2, 3]);
            graph.add_node([3]);
            graph.add_node([1]);
            graph
        };

        assert_eq!(detector.detect(0, &graph2), Some(vec![3, 2, 1]));
    }
}
