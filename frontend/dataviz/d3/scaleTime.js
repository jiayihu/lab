(async function() {
  const parseTime = d3.timeParse('%m/%d/%y');
  const formatTime = d3.timeFormat('%e');
  const dataset = await d3.csv('time_scale_data.csv', d => {
    return {
      Date: parseTime(d.Date),
      Amount: Number(d.Amount)
    };
  });

  /**
   * Mike Bostock's margin convention
   * @see {@link https://bl.ocks.org/mbostock/3019563}
   */
  const margin = { top: 20, right: 40, bottom: 20, left: 30 };
  const width = 600 - margin.left - margin.right;
  const height = 300 - margin.top - margin.bottom;
  const svg = d3
    .select('.scaleTime')
    .append('svg')
    .style('width', width + margin.left + margin.right)
    .style('height', height + margin.top + margin.bottom)
    .append('g')
    .attr('transform', `translate(${margin.left}, ${margin.top})`);

  const xScale = d3
    .scaleTime()
    .domain([d3.min(dataset, d => d.Date), d3.max(dataset, d => d.Date)])
    .range([0, width]);
  const yScale = d3
    .scaleLinear()
    .domain([d3.min(dataset, d => d.Amount), d3.max(dataset, d => d.Amount)])
    .range([height, 0]);

  svg
    .selectAll('line')
    .data(dataset)
    .enter()
    .append('line')
    .attr('x1', function(d) {
      return xScale(d.Date);
    })
    .attr('x2', function(d) {
      return xScale(d.Date);
    })
    .attr('y1', height)
    .attr('y2', function(d) {
      return yScale(d.Amount);
    })
    .attr('stroke', '#ddd')
    .attr('stroke-width', 1);

  svg
    .selectAll('circle')
    .data(dataset)
    .enter()
    .append('circle')
    .attr('cx', d => xScale(d.Date))
    .attr('cy', d => yScale(d.Amount))
    .attr('r', 3);

  const xAxis = d3.axisBottom(xScale).tickFormat(formatTime);
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
