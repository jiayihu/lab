(function() {
  const dataset = [
    25,
    7,
    5,
    26,
    11,
    8,
    25,
    14,
    23,
    19,
    14,
    11,
    22,
    29,
    11,
    13,
    12,
    17,
    18,
    10,
    24,
    18,
    25,
    9,
    3
  ];

  /**
   * Mike Bostock's margin convention
   * @see {@link https://bl.ocks.org/mbostock/3019563}
   */
  const margin = { top: 20, right: 40, bottom: 20, left: 30 };
  const width = 600 - margin.left - margin.right;
  const height = 250 - margin.top - margin.bottom;
  const svg = d3
    .select('.interactive-barchart')
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
    .domain([0, d3.max(dataset)])
    .range([0, height]);

  function renderRects(rects) {
    rects
      .attr('x', (d, i) => xScale(i))
      .attr('y', d => height - yScale(d))
      .attr('width', xScale.bandwidth())
      .attr('height', d => yScale(d))
      .style('fill', d => `rgb(${d * 10}, 0, 0)`);
  }

  const rects = svg
    .selectAll('rect')
    .data(dataset)
    .enter()
    .append('rect');
  renderRects(rects);

  rects.on('mouseover', function(d) {
    const rect = d3.select(this);
    const x = Number(rect.attr('x')) + xScale.bandwidth() / 2;
    const y = Number(rect.attr('y')) + 14;

    svg
      .append('text')
      .attr('id', 'tooltip')
      .attr('x', x)
      .attr('y', y)
      .attr('text-anchor', 'middle')
      .attr('font-size', '11px')
      .attr('font-weight', 'bold')
      .attr('fill', 'white')
      .text(d);
  });
  rects.on('mouseout', () => {
    svg.select('#tooltip').remove();
  });

  let isAscending = false;

  d3.select('.interactive-barchart')
    .select('button')
    .on('click', () => {
      svg
        .selectAll('rect')
        .sort((a, b) => (isAscending ? d3.descending(a, b) : d3.ascending(a, b)))
        .transition()
        .attr('x', (d, i) => xScale(i));

      isAscending = !isAscending;
    });
})();
