;(async function() {
  const margin = { top: 80, right: 0, bottom: 30, left: 60 }
  const width = 900 - margin.left - margin.right
  const height = 600 - margin.top - margin.bottom
  const svg = d3
    .select('.unemployment')
    .append('svg')
    .style('width', width + margin.left + margin.right)
    .style('height', height + margin.top + margin.bottom)
    .append('g')
    .attrs({
      transform: `translate(${margin.left}, ${margin.top})`,
      color: '#616161',
      fill: '#616161',
      'font-size': '13px',
    })

  const unemployment = await d3.csv('unemployment.csv', d => {
    return {
      date: new Date(d.date),
      rate: Number(d.rate),
    }
  })
  const dataset = d3
    .nest()
    .key(d => d.date.getFullYear())
    .entries(unemployment)

  const [minYear, maxYear] = d3.extent(dataset, d => Number(d.key))
  const xScale = d3
    .scaleBand()
    .domain(d3.range(minYear, maxYear + 1))
    .range([0, width])
    .paddingInner(0.05)
  const yScale = d3
    .scaleBand()
    .domain(d3.range(1, 13))
    .range([0, height])
    .paddingInner(0.01)
  const colorScale = d3
    .scaleQuantize()
    .domain(d3.extent(unemployment, d => d.rate))
    .range(['#fef0d9', '#fdcc8a', '#fc8d59', '#e34a33', '#b30000'])

  const years = svg
    .selectAll('g')
    .data(dataset)
    .enter()
    .append('g')
    .attr('class', d => d.key)

  years
    .selectAll('rect')
    .data(d => d.values)
    .enter()
    .append('rect')
    .attrs({
      x: d => xScale(d.date.getFullYear()),
      y: d => yScale(d.date.getMonth() + 1),
      width: xScale.bandwidth(),
      height: yScale.bandwidth(),
      fill: d => colorScale(d.rate),
    })
    .append('title')
    .text(d => `${d.rate}%`)

  const months = [
    'January',
    'February',
    'March',
    'April',
    'May',
    'June',
    'July',
    'August',
    'September',
    'October',
    'November',
    'December',
  ]
  const xAxis = d3.axisTop(xScale)
  const yAxis = d3.axisLeft(yScale).tickFormat(monthIndex => months[monthIndex - 1])

  const xAxisEl = svg
    .append('g')
    .attr('transform', `translate(0, 0)`)
    .call(xAxis)
  const yAxisEl = svg.append('g').call(yAxis)

  xAxisEl
    .selectAll('text')
    .attr('transform', 'rotate(-45)')
    .style('text-anchor', 'initial')

  xAxisEl.select('.domain').style('display', 'none')
  xAxisEl.selectAll('line').style('display', 'none')

  yAxisEl.select('.domain').style('display', 'none')
  yAxisEl.selectAll('line').style('display', 'none')

  // Legend
  const legend = d3
    .legendColor()
    .orient('horizontal')
    .scale(colorScale)
    .shapePadding(70)
    .shapeWidth(12)
    .shapeHeight(12)
  svg
    .append('g')
    .attr('transform', `translate(${width / 2}, -70)`)
    .call(legend)

  svg
    .append('text')
    .text('Historical unemployment in Italy')
    .attr('transform', `translate(0, ${-margin.top / 2})`)
    .styles({
      'font-size': 24,
      'font-weight': 'bold',
    })

  svg
    .append('text')
    .text('Source: OECD (2018)')
    .attr('transform', `translate(${-margin.left}, ${height + margin.bottom})`)
    .styles({
      'font-size': 10,
    })
})()
