(function() {
  const dataset = [
    [5, 20],
    [480, 90],
    [250, 50],
    [100, 33],
    [330, 95],
    [410, 12],
    [475, 44],
    [25, 67],
    [85, 21],
    [220, 88],
    [600, 150],
  ];

  /**
   * Mike Bostock's margin convention
   * @see {@link https://bl.ocks.org/mbostock/3019563}
   */
  const margin = { top: 20, right: 40, bottom: 20, left: 30 };
  const width = 600 - margin.left - margin.right;
  const height = 300 - margin.top - margin.bottom;
  const svg = d3
    .select('.scatterplot')
    .append('svg')
    .style('width', width + margin.left + margin.right)
    .style('height', height + margin.top + margin.bottom)
    .append('g')
    .attr('transform', `translate(${margin.left}, ${margin.top})`);

  const xScale = d3
    .scaleLinear()
    .domain([0, d3.max(dataset, d => d[0])])
    .range([0, width]);
  const yScale = d3
    .scaleLinear()
    .domain([0, d3.max(dataset, d => d[1])])
    .range([height, 0]);
  const aScale = d3
    .scaleSqrt()
    .domain([0, d3.max(dataset, d => d[1])])
    .range([0, 10]);

  svg
    .selectAll('circle')
    .data(dataset)
    .enter()
    .append('circle')
    .attr('cx', d => xScale(d[0]))
    .attr('cy', d => yScale(d[1]))
    .attr('r', d => aScale(d[1]));

  const xAxis = d3.axisBottom(xScale).ticks(5);
  svg
    .append('g')
    .classed('axis', true)
    .attr('transform', `translate(0, ${height})`)
    .call(xAxis);

  const yAxis = d3.axisLeft(yScale).ticks(5);
  svg
    .append('g')
    .classed('axis', true)
    .call(yAxis);
})();
