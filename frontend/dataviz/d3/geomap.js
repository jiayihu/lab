(async function() {
  const dataset = await d3.json('us-states.json');

  /**
   * Mike Bostock's margin convention
   * @see {@link https://bl.ocks.org/mbostock/3019563}
   */
  const margin = { top: 20, right: 40, bottom: 20, left: 30 };
  const width = 900 - margin.left - margin.right;
  const height = 500 - margin.top - margin.bottom;
  const svg = d3
    .select('.geomap')
    .append('svg')
    .style('width', width + margin.left + margin.right)
    .style('height', height + margin.top + margin.bottom)
    .append('g')
    .attr('transform', `translate(${margin.left}, ${margin.top})`);

  const projection = d3.geoAlbersUsa().translate([width / 2, height / 2]);
  const path = d3.geoPath().projection(projection);

  svg
    .selectAll('path')
    .data(dataset.features)
    .enter()
    .append('path')
    .attr('d', path)
    .attr('fill', 'cadetblue')
    .attr('stroke', 'rgba(255,255,255,.1)')
    .attr('stroke-width', 1);
})();
