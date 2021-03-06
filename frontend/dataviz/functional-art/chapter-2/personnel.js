;(async function() {
  const margin = { top: 30, right: 80, bottom: 50, left: 70 }
  const width = 900 - margin.left - margin.right
  const height = 600 - margin.top - margin.bottom

  const svg = d3
    .select('.personnel')
    .append('svg')
    .style('width', width + margin.left + margin.right)
    .style('height', height + margin.top + margin.bottom)
    .append('g')
    .attrs({
      transform: `translate(${margin.left}, ${margin.top})`,
      color: '#616161',
      fill: '#616161',
      'font-size': '12px',
    })

  // const container = svg.append('g')
  // const zoom = d3
  //   .zoom()
  //   .scaleExtent([1, 10])
  //   .translateExtent([[-900, -600], [900, 600]])
  //   .on('zoom', function() {
  //     svg.attr('transform', d3.event.transform)
  //   })
  // svg.call(zoom)

  // // Fake rect to trigger zoom wherever
  // svg
  //   .append('rect')
  //   .attr('width', width)
  //   .attr('height', height)
  //   .style('fill', 'none')
  //   .style('pointer-events', 'all')

  const personnel = await d3.csv('./armed-forces-personnel.csv', d => {
    return {
      country: d.country,
      code: d.code,
      personnel: Number(d['2016']),
    }
  })
  const population = await d3.csv('./population.csv', d => {
    return {
      country: d.country,
      code: d.code,
      population: Number(d.population),
    }
  })
  const expenditure = await d3.csv('./expenditure.csv', d => {
    return {
      country: d.country,
      code: d.code,
      expenditure: Number(d.expenditure),
    }
  })

  const zip = (xs, ...yss) => xs.map((x, i) => [x, ...yss.map(ys => ys[i])])
  const dataset = zip(personnel, population, expenditure).map(([x, y, z]) => {
    return {
      ...x,
      population: y.population,
      expenditure: z.expenditure,
    }
  })

  const populationMinMax = d3.extent(dataset, d => d.population)
  const personnelMinMax = d3.extent(dataset, d => d.personnel)
  const expenditureMinMax = d3.extent(dataset, d => d.expenditure)
  const xScale = d3
    .scaleLinear()
    .domain([0, populationMinMax[1]])
    .range([0, width])
  const yScale = d3
    .scaleLinear()
    .domain(personnelMinMax)
    .range([height, 0])
  const areaScale = d3
    .scaleSqrt()
    .domain(expenditureMinMax)
    .range([3, 20])

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

  const formatExpenditure = d3.format('.2r')
  svg
    .append('g')
    .selectAll('circle')
    .data(dataset)
    .enter()
    .append('circle')
    .attrs({
      cx: d => xScale(d.population),
      cy: d => yScale(d.personnel),
      r: d => areaScale(d.expenditure),
      fill: d3.schemePastel1[0],
    })
    .append('title')
    .text(d => `${d.country} military expenditure: ${formatExpenditure(d.expenditure)}% of GDP`)

  svg
    .append('g')
    .selectAll('circle')
    .data(dataset)
    .enter()
    .append('circle')
    .attrs({
      cx: d => xScale(d.population),
      cy: d => yScale(d.personnel),
      r: 1,
      fill: 'white',
    })

  svg
    .append('g')
    .selectAll('text')
    .data(dataset)
    .enter()
    .filter(d => d.personnel > 50000)
    .append('text')
    .text(d => d.country)
    .attrs({
      x: d => xScale(d.population) + areaScale(d.expenditure),
      y: d => yScale(d.personnel),
    })

  const regression = leastSquares(dataset.map(x => x.population), dataset.map(x => x.personnel))
  svg.append('line').attrs({
    x1: xScale(populationMinMax[0]),
    y1: yScale(regression(populationMinMax[0])),
    x2: xScale(populationMinMax[1]),
    y2: yScale(regression(populationMinMax[1])),

    stroke: d3.schemePastel2[7],
  })

  const xAxis = d3.axisBottom(xScale)
  const yAxis = d3.axisLeft(yScale)

  svg
    .append('g')
    .attr('transform', `translate(0, ${height})`)
    .call(xAxis)
  svg.append('g').call(yAxis)

  // Axis names
  svg
    .append('text')
    .attr('transform', `translate(${width}, ${height + 40})`)
    .attr('text-anchor', 'end')
    .text('POPULATION (2016)')
  svg
    .append('text')
    .attr('transform', `translate(-${margin.left}, 0), rotate(90)`)
    .text('ARMED FORCES PERSONNEL (2016)')

  // Annotation
  const type = d3.annotationCustomType(d3.annotationCalloutElbow, {
    className: 'custom',
    connector: { type: 'elbow' },
    note: {
      lineType: 'vertical',
      align: 'middle',
    },
  })

  const annotations = [
    {
      note: {
        label: 'The closer the circles are to this line, the more correlated the variables are.',
        title: 'Trend line',
        wrap: 150,
      },
      data: { population: 55000000, personnel: regression(55000000) },
      dy: -25,
      dx: -200,
    },
  ]

  const makeAnnotations = d3
    .annotation()
    .accessors({
      x: d => xScale(d.population),
      y: d => yScale(d.personnel),
    })
    .notePadding(10)
    .type(type)
    .annotations(annotations)

  svg.append('g').call(makeAnnotations)

  // Legend
  const legend = d3
    .legendSize()
    .scale(areaScale)
    .shape('circle')
    .shapePadding(30)
    .orient('horizontal')
    .labels(({ i, generatedLabels }) => `${generatedLabels[i]}%`)
    .labelOffset(15)
    .title('Size of circles proportional to the budget as % of the GDP')
  svg
    .append('g')
    .attr('transform', `translate(50, 50)`)
    .call(legend)

  // Data source
  svg
    .append('text')
    .text('Source: World Bank')
    .attr('transform', `translate(${-margin.left}, ${height + margin.bottom})`)
    .styles({
      'font-size': 10,
    })

  function leastSquares(XaxisData, Yaxisdata) {
    const ReduceAddition = function(prev, cur) {
      return prev + cur
    }

    // finding the mean of Xaxis and Yaxis data
    const xBar = (XaxisData.reduce(ReduceAddition) * 1.0) / XaxisData.length
    const yBar = (Yaxisdata.reduce(ReduceAddition) * 1.0) / Yaxisdata.length

    const SquareXX = XaxisData.map(function(d) {
      return Math.pow(d - xBar, 2)
    }).reduce(ReduceAddition)

    const MeanDiffXY = XaxisData.map(function(d, i) {
      return (d - xBar) * (Yaxisdata[i] - yBar)
    }).reduce(ReduceAddition)

    const slope = MeanDiffXY / SquareXX
    const intercept = yBar - xBar * slope

    // returning regression function
    return x => x * slope + intercept
  }
})()
