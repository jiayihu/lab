(function() {
  const dataset = [
    { apples: 5, oranges: 10, grapes: 22 },
    { apples: 4, oranges: 12, grapes: 28 },
    { apples: 2, oranges: 19, grapes: 32 },
    { apples: 7, oranges: 23, grapes: 35 },
    { apples: 23, oranges: 17, grapes: 43 }
  ];

  /**
   * Mike Bostock's margin convention
   * @see {@link https://bl.ocks.org/mbostock/3019563}
   */
  const margin = { top: 20, right: 40, bottom: 20, left: 30 };
  const width = 600 - margin.left - margin.right;
  const height = 250 - margin.top - margin.bottom;
  const svg = d3
    .select('.stack')
    .append('svg')
    .style('width', width + margin.left + margin.right)
    .style('height', height + margin.top + margin.bottom)
    .append('g')
    .attr('transform', `translate(${margin.left}, ${margin.top})`);

  const xScale = d3
    .scaleBand()
    .domain(d3.range(dataset.length))
    .range([0, width])
    .round(true)
    .paddingInner(0.05);
  const yScale = d3
    .scaleLinear()
    .domain([0, d3.max(dataset.map(x => x.apples + x.oranges + x.grapes))])
    .range([height, 0]);

  const stack = d3.stack().keys(['apples', 'oranges', 'grapes']);
  const colors = d3.scaleOrdinal(d3.schemeCategory10);

  const groups = svg
    .selectAll('g')
    .data(stack(dataset))
    .enter()
    .append('g')
    .style('fill', (d, i) => colors(i));

  const rects = groups
    .selectAll('rect')
    .data(d => d)
    .enter()
    .append('rect')
    .attr('x', (d, i) => xScale(i))
    .attr('y', d => yScale(d[1]))
    .attr('height', d => yScale(d[0]) - yScale(d[1]))
    .attr('width', xScale.bandwidth());
})();
