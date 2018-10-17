(function() {
  const dataset = [5, 10, 20, 45, 6, 25];

  /**
   * Mike Bostock's margin convention
   * @see {@link https://bl.ocks.org/mbostock/3019563}
   */
  const margin = { top: 20, right: 40, bottom: 20, left: 30 };
  const width = 300 - margin.left - margin.right;
  const height = 300 - margin.top - margin.bottom;
  const svg = d3
    .select('.pie')
    .append('svg')
    .style('width', width + margin.left + margin.right)
    .style('height', height + margin.top + margin.bottom)
    .append('g')
    .attr('transform', `translate(${margin.left}, ${margin.top})`);

  const outerRadius = width / 2;
  const innerRadius = width / 4;
  const arc = d3
    .arc()
    .innerRadius(innerRadius)
    .outerRadius(outerRadius);

  const pie = d3.pie();
  const color = d3.scaleOrdinal(d3.schemeCategory10);

  const arcs = svg
    .selectAll('g.arc')
    .data(pie(dataset))
    .enter()
    .append('g')
    .attr('class', 'arc')
    .attr('transform', `translate(${outerRadius}, ${outerRadius})`);

  arcs
    .append('path')
    .attr('d', arc)
    .attr('fill', (d, i) => color(i));

  arcs
    .append('text')
    .attr('transform', d => `translate(${arc.centroid(d)})`)
    .style('text-anchor', 'middle')
    .style('fill', 'white')
    .text(d => d.value);
})();
