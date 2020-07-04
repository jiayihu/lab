(function() {
  const dataset = {
    nodes: [
      { name: 'Adam' },
      { name: 'Bob' },
      { name: 'Carrie' },
      { name: 'Donovan' },
      { name: 'Edward' },
      { name: 'Felicity' },
      { name: 'George' },
      { name: 'Hannah' },
      { name: 'Iris' },
      { name: 'Jerry' }
    ],
    edges: [
      { source: 0, target: 1 },
      { source: 0, target: 2 },
      { source: 0, target: 3 },
      { source: 0, target: 4 },
      { source: 1, target: 5 },
      { source: 2, target: 5 },
      { source: 2, target: 5 },
      { source: 3, target: 4 },
      { source: 5, target: 8 },
      { source: 5, target: 9 },
      { source: 6, target: 7 },
      { source: 7, target: 8 },
      { source: 8, target: 9 }
    ]
  };

  /**
   * Mike Bostock's margin convention
   * @see {@link https://bl.ocks.org/mbostock/3019563}
   */
  const margin = { top: 20, right: 40, bottom: 20, left: 30 };
  const width = 600 - margin.left - margin.right;
  const height = 250 - margin.top - margin.bottom;
  const svg = d3
    .select('.graph')
    .append('svg')
    .style('width', width + margin.left + margin.right)
    .style('height', height + margin.top + margin.bottom)
    .append('g')
    .attr('transform', `translate(${margin.left}, ${margin.top})`);

  const force = d3
    .forceSimulation(dataset.nodes)
    .force('change', d3.forceManyBody())
    .force('link', d3.forceLink(dataset.edges))
    .force(
      'center',
      d3
        .forceCenter()
        .x(width / 2)
        .y(height / 2)
    );

  const colors = d3.schemeCategory10;

  const edges = svg
    .selectAll('line')
    .data(dataset.edges)
    .enter()
    .append('line')
    .attr('stroke', '#ccc')
    .attr('stroke-width', 1);
  const nodes = svg
    .selectAll('circle')
    .data(dataset.nodes)
    .enter()
    .append('circle')
    .attr('r', 10)
    .attr('fill', (d, i) => colors[i]);

  nodes.append('title').text(d => d.name);

  force.on('tick', () => {
    edges.attr('x1', d => d.source.x);
    edges.attr('y1', d => d.source.y);
    edges.attr('x2', d => d.target.x);
    edges.attr('y2', d => d.target.y);

    nodes.attr('cx', d => d.x);
    nodes.attr('cy', d => d.y);
  });

  nodes.call(
    d3
      .drag()
      .on('start', d => {
        if (!d3.event.active) force.alphaTarget(0.3).restart();
        d.fx = d.x;
        d.fy = d.y;
      })
      .on('drag', d => {
        d.fx = d3.event.x;
        d.fy = d3.event.y;
      })
      .on('end', d => {
        if (!d3.event.active) force.alphaTarget(0);
        d.fx = null;
        d.fy = null;
      })
  );
})();
