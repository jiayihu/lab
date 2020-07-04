;(async function() {
  const margin = { top: 20, right: 30, bottom: 100, left: 50 }
  const width = 900 - margin.left - margin.right
  const height = 600 - margin.top - margin.bottom
  const svg = d3
    .select('.gdp-gini')
    .append('svg')
    .style('width', width + margin.left + margin.right)
    .style('height', height + margin.top + margin.bottom)
    .append('g')
    .attr('transform', `translate(${margin.left}, ${margin.top})`)
    .attr('color', '#616161')
    .attr('fill', '#616161')
    .attr('font-size', '13px')

  const gdpCSV = await d3.text('gdp.csv')
  const [gdpYears, gdp] = d3.csvParseRows(gdpCSV, (d, i) => {
    return i === 0 ? d.map(Number) : d.map(x => Number(x) / 1000000000)
  })

  const giniCSV = await d3.text('gini.csv')
  const [giniYears, gini] = d3.csvParseRows(giniCSV, d => d.map(x => Number(x)))

  const governmentCSV = await d3.text('government.csv')
  const [governmentYears, governments] = d3.csvParseRows(governmentCSV)

  const zip = (xs, ...yss) => xs.map((x, i) => [x, ...yss.map(ys => ys[i])])
  const dataset = zip(governments, gdpYears, gdp, gini).map(([government, year, gdp, gini]) => {
    return { government, year, gdp, gini }
  })

  const xScale = d3
    .scaleLinear()
    .domain(d3.extent(gdp))
    .range([0, width])
  const yScale = d3
    .scaleLinear()
    .domain(d3.extent(gini))
    .range([height, 0])

  const uniqueGoverns = Array.from(new Set(governments))
  const colorScale = d3
    .scaleOrdinal()
    .domain(uniqueGoverns)
    .range(d3.schemeCategory10.slice(0, uniqueGoverns.length))

  // Grid
  svg
    .append('g')
    .selectAll('line')
    .data(yScale.ticks())
    .enter()
    .append('line')
    .attr('x1', 0)
    .attr('y1', d => yScale(d))
    .attr('x2', width)
    .attr('y2', d => yScale(d))
    .attr('fill', 'none')
    .attr('stroke', '#BDBDBD')
    .attr('stroke-dasharray', 2)
  svg
    .append('g')
    .selectAll('line')
    .data(xScale.ticks())
    .enter()
    .append('line')
    .attr('x1', d => xScale(d))
    .attr('y1', 0)
    .attr('x2', d => xScale(d))
    .attr('y2', height)
    .attr('fill', 'none')
    .attr('stroke', '#BDBDBD')
    .attr('stroke-dasharray', 2)

  const context = {
    index: 0,
    subpaths: [],
    moveTo: '',

    moveTo: function(x, y) {
      this.moveTo = `M ${x} ${y}`
    },
    lineTo: function(x, y) {
      this.subpaths.push({
        government: dataset[this.index].government,
        d: `${this.moveTo} L ${x} ${y}`,
      })
      this.moveTo = `M ${x} ${y}`
    },
    bezierCurveTo: function(cp1x, cp1y, cp2x, cp2y, x, y) {
      this.subpaths.push({
        government: dataset[this.index].government,
        d: `${this.moveTo} C ${cp1x} ${cp1y}, ${cp2x} ${cp2y}, ${x} ${y}`,
      })
      this.moveTo = `M ${x} ${y}`
      this.index += 1
    },
  }

  // Main line
  const line = d3
    .line()
    .curve(d3.curveCardinal)
    .x(d => xScale(d.gdp))
    .y(d => yScale(d.gini))
    .context(context)
  line(dataset)

  svg
    .append('g')
    .selectAll('path')
    .data(context.subpaths)
    .enter()
    .append('path')
    .attr('d', d => d.d)
    .attr('fill', 'none')
    .attr('stroke', d => colorScale(d.government))
    .attr('stroke-width', '2px')

  // Years
  svg
    .append('g')
    .selectAll('circle')
    .data(dataset)
    .enter()
    .append('circle')
    .attr('cx', d => xScale(d.gdp))
    .attr('cy', d => yScale(d.gini))
    .attr('r', 3)
  svg
    .append('g')
    .selectAll('text')
    .data(dataset)
    .enter()
    .append('text')
    .text(d => d.year)
    .attr('x', d => xScale(d.gdp))
    .attr('y', d => yScale(d.gini))
    .style('font-size', '11px')

  const formatGdp = d3.format(',.2r')
  const xAxis = d3.axisBottom(xScale).tickFormat(formatGdp)
  const yAxis = d3.axisLeft(yScale)

  svg
    .append('g')
    .attr('transform', `translate(0, ${height})`)
    .call(xAxis)
  svg.append('g').call(yAxis)

  // Axis names
  svg
    .append('text')
    .attr('transform', `translate(${width}, ${height + 30})`)
    .attr('text-anchor', 'end')
    .text('GDP (billions)')
  svg
    .append('text')
    .attr('transform', `translate(-${margin.left}, 0), rotate(90)`)
    .text('GINI Index')

  // Legend
  const legend = d3
    .legendColor()
    .orient('horizontal')
    .scale(colorScale)
    .shapePadding(50)
    .shapeWidth(12)
    .shapeHeight(12)
  svg
    .append('g')
    .attr('transform', `translate(100, ${height + 30})`)
    .call(legend)
})()
