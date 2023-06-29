use std::{iter, ops::Range};

use crate::BitSet;

#[derive(Debug)]
pub struct Graph {
    ranges: Vec<usize>,
    edges: Vec<usize>,
}

impl Default for Graph {
    fn default() -> Self {
        Self::new()
    }
}

impl Graph {
    pub fn new() -> Self {
        Self {
            ranges: vec![0],
            edges: Vec::new(),
        }
    }

    fn clear(&mut self) {
        self.ranges.truncate(1);
        self.edges.clear();
    }

    pub fn add_node(&mut self, children: impl IntoIterator<Item = usize>) {
        self.edges.extend(children);
        self.ranges.push(self.edges.len());
    }

    pub fn add_node_and_dedup(&mut self, children: impl IntoIterator<Item = usize>) {
        let start = self.edges.len();
        self.edges.extend(children);
        self.edges[start..].sort_unstable();
        self.edges.dedup();
        self.ranges.push(self.edges.len());
    }

    fn get_children(&self, node: usize) -> &[usize] {
        // get end first to avoid bounds check
        &self.edges[self.get_children_range(node)]
    }

    fn get_children_range(&self, node: usize) -> Range<usize> {
        // get end first to avoid bounds check
        let end = self.ranges[node + 1];
        let start = self.ranges[node];
        start..end
    }

    fn node_count(&self) -> usize {
        self.ranges.len() - 1
    }

    fn iter(&self) -> impl Iterator<Item = &[usize]> {
        self.ranges
            .array_windows()
            .map(|&[start, end]| &self.edges[start..end])
    }
}

#[derive(Default)]
pub struct CycleFinder {
    // the graph is also usefull for collecting integer groups in compact way
    cycles: Graph,
    index: Vec<usize>,
    lowlink: Vec<usize>,
    stack: Vec<usize>,
    on_stack: BitSet,
}

impl CycleFinder {
    const UNIDENTIFIED: usize = usize::MAX;

    pub fn collect_cycles(&mut self, graph: &Graph) {
        self.cycles.clear();
        self.index.clear();
        self.lowlink.clear();

        self.index.resize(graph.node_count(), Self::UNIDENTIFIED);
        self.lowlink.resize(graph.node_count(), Self::UNIDENTIFIED);
        self.on_stack.resize(graph.node_count());

        let mut index = 0;
        for node in 0..graph.node_count() {
            if self.index[node] == Self::UNIDENTIFIED {
                self.collect_cycles_from(graph, node, &mut index);
            }
        }
    }

    pub fn join_all_cycles_into(&mut self, source_graph: &Graph, dest_graph: &mut Graph) {
        self.collect_cycles(source_graph);
        for (i, group) in self.cycles.iter().enumerate() {
            for &node in group {
                self.index[node] = i;
            }
        }

        for (i, group) in self.cycles.iter().enumerate() {
            let edge_iter = group
                .iter()
                .flat_map(|&node| source_graph.get_children(node))
                .copied()
                .filter(|&child| self.index[child] != i)
                .map(|child| self.index[child]);
            dest_graph.add_node_and_dedup(edge_iter);
        }
    }

    fn collect_cycles_from(&mut self, graph: &Graph, root: usize, index: &mut usize) {
        self.index[root] = *index;
        self.lowlink[root] = *index;
        *index += 1;
        self.stack.push(root);
        self.on_stack.insert(root);

        for &child in graph.get_children(root) {
            if self.index[child] == Self::UNIDENTIFIED {
                self.collect_cycles_from(graph, child, index);
                self.lowlink[root] = self.lowlink[root].min(self.lowlink[child]);
            } else if self.on_stack.contains(child) {
                self.lowlink[root] = self.lowlink[root].min(self.index[child]);
            }
        }

        if self.lowlink[root] == self.index[root] {
            let mut seen_root = false;
            self.cycles.add_node(iter::from_fn(|| {
                if seen_root {
                    return None;
                }
                let node = self.stack.pop()?;
                self.on_stack.remove(node);
                seen_root = node == root;
                Some(node)
            }));
        }
    }
}

pub struct CycleDetector {
    stack: Vec<Range<usize>>,
    order: Vec<usize>,
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

    pub fn detect(&mut self, root: usize, graph: &Graph) -> Option<Vec<usize>> {
        self.preapre(graph);
        self.detect_from(root, graph)
    }

    pub fn detect_all(&mut self, graph: &Graph) -> Vec<Vec<usize>> {
        self.preapre(graph);
        let mut result = Vec::new();
        for node in 0..graph.node_count() {
            if !self.seen.contains(node) {
                if let Some(cycle) = self.detect_from(node, graph) {
                    for &node in &cycle {
                        self.seen.insert(node);
                    }
                    result.push(cycle);
                    self.in_stack.clear();
                }
            }
        }
        result
    }

    fn preapre(&mut self, graph: &Graph) {
        self.seen.clear();
        self.seen.resize(graph.node_count());
        self.order.clear();
    }

    fn detect_from(&mut self, root: usize, graph: &Graph) -> Option<Vec<usize>> {
        self.in_stack.insert(root);
        self.stack.push(graph.get_children_range(root));

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
                self.in_stack.clear();
                let cycle = self
                    .stack
                    .drain(..)
                    .rev()
                    .skip(1)
                    .map(|r| graph.edges[r.start as usize - 1])
                    .take_while(|r| *r != current)
                    .chain(iter::once(current))
                    .collect();
                return Some(cycle);
            }

            self.stack.push(graph.get_children_range(current));
        }

        self.order.push(root);

        None
    }

    pub fn order(&self) -> &[usize] {
        &self.order
    }
}

#[cfg(test)]
mod test {
    use super::{CycleDetector, Graph};

    fn perform_test(graph_source: &str, ctx: &mut String) {
        let graph = parse_graph(graph_source);
        let mut cycle_finder = super::CycleFinder::default();
        let mut merged_graph = Graph::new();
        cycle_finder.join_all_cycles_into(&graph, &mut merged_graph);

        for (cycle, children) in cycle_finder.cycles.iter().zip(merged_graph.iter()) {
            ctx.push_str(&format!("{:?} -> {:?}\n", cycle, children));
        }
    }

    crate::print_cases! { perform_test:
        single_node "";
        self_referential "0";
        two_nodes "
            1
            ;
        ";
        two_nodes_cycle "
            1
            0
        ";
        parallel_cycles "
            1
            0
            3 1
            2 0
        ";
        complex_cycle "
            1 2
            2 0
            0 1
            0
        ";
        cycle_with_subcycles "
            1
            0 2
            3
            2 0
        ";
    }

    fn perform_cycle_detect_test(souce: &str, ctx: &mut String) {
        let graph = parse_graph(souce);
        let mut cycle_detector = CycleDetector::new();
        let cycles = cycle_detector.detect_all(&graph);
        if cycles.is_empty() {
            ctx.push_str("No cycles found\n");
            ctx.push_str(&format!("Order: {:?}\n", cycle_detector.order()));
        } else {
            for cycle in cycles {
                ctx.push_str(&format!("{:?}\n", cycle));
            }
        }
    }

    crate::print_cases! { perform_cycle_detect_test:
        no_cycles_descen "
            1 2 3
            2 3
            3
            ;
        ";
        no_cycles_ascen "
            1 3 3
            3
            1 3
            ;
        ";
        cycles_descend "
            1 2 3
            2 3
            3
            0
        ";
        cycles_complex "
            1 2 3
            2 3
            3
            1
        ";
        cycle_self_reference "0";
    }

    fn parse_graph(graph_source: &str) -> Graph {
        let mut graph = Graph::new();
        for line in graph_source.trim().split('\n') {
            let iter = line.split_whitespace().filter_map(|s| s.parse().ok());
            graph.add_node(iter);
        }
        graph
    }
}
