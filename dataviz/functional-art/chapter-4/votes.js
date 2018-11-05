;(async function() {
  const margin = { top: 40, right: 40, bottom: 40, left: 40 }
  const w = 600 - margin.left - margin.right
  const h = 600 - margin.top - margin.bottom

  const svg = d3
    .select('.votes')
    .append('svg')
    .style('width', w + margin.left + margin.right)
    .style('height', h + margin.top + margin.bottom)
    .append('g')
    .attrs({
      transform: `translate(${margin.left}, ${margin.top})`,
      color: '#616161',
      fill: '#616161',
      'font-size': '12px',
    })

  const parties = ['lega', 'movimento 5 stelle', 'partito democratico']
  const votes = (await d3.csv('./votes.csv', d => {
    return {
      region: d.region,
      list: d.list,
      votes: Number(d.votes),
    }
  }))
    .filter(d => parties.includes(d.list))
    .reduce((map, d) => {
      if (!map[d.region]) map[d.region] = { region: d.region }

      map[d.region][d.list] = d.votes

      return map
    }, {})
  const dataset = Object.keys(votes).map(region => votes[region])
  const stack = d3
    .stack()
    .keys(parties)
    .offset(d3.stackOffsetWiggle)
  const series = stack(dataset)

  const max = d3.max(dataset, d => d3.max(parties.map(party => d[party])))
  const axis = dataset.map(d => d.region)
  const radius = Math.min(w / 2, h / 2)
  const angle = (2 * Math.PI) / axis.length
  const center = { x: w / 2, y: h / 2 }
  const formatVotes = d3.format('.4r')

  // Circular segments
  const levels = 6
  const segmentsG = svg.append('g').attr('class', 'circular-segments')

  segmentsG
    .selectAll('polygon')
    .data(d3.range(1, 6))
    .enter()
    .append('polygon')
    .attrs(d => {
      const levelRadius = (radius * d) / levels
      const points = axis
        .map((_, i) => {
          const x = (1 - Math.sin(i * angle)) * levelRadius
          const y = (1 - Math.cos(i * angle)) * levelRadius

          return `${x},${y}`
        })
        .join(' ')

      return {
        fill: 'none',
        points,
        transform: `translate(${w / 2 - levelRadius}, ${h / 2 - levelRadius})`,
      }
    })
    .styles({
      stroke: d3.schemePastel1[8],
    })

  segmentsG
    .selectAll('text')
    .data(d3.range(1, 6))
    .enter()
    .append('text')
    .attrs(d => {
      const levelRadius = (radius * d) / levels
      const x = (1 - Math.sin(0)) * levelRadius
      const y = (1 - Math.cos(0)) * levelRadius

      return {
        x,
        y,
        transform: `translate(${w / 2 - levelRadius}, ${h / 2 - levelRadius})`,
      }
    })
    .styles({
      fill: d3.schemePastel2[7],
      'font-size': '10px',
    })
    .text(d => formatVotes((d * max) / levels))

  // Axis
  const axisG = svg.append('g').attr('class', 'axis')

  axisG
    .selectAll('line')
    .data(axis)
    .enter()
    .append('line')
    .attrs((d, i) => ({
      x1: center.x,
      y1: center.y,
      x2: (1 - Math.sin(i * angle)) * radius,
      y2: (1 - Math.cos(i * angle)) * radius,
    }))
    .styles({
      stroke: d3.schemePastel1[8],
    })

  axisG
    .selectAll('text')
    .data(axis)
    .enter()
    .append('text')
    .attrs((d, i) => {
      const sin = Math.sin(i * angle)
      const cos = Math.cos(i * angle)
      return {
        x: (1 - sin) * radius,
        y: (1 - cos) * radius - 10 * cos,
      }
    })
    .styles({
      fill: d3.schemeCategory10[7],
      'font-size': '10px',
      'text-anchor': 'middle',
    })
    .text(d => d)

  // Areas
  const areaG = svg.append('g').attr('class', 'area')

  areaG
    .selectAll('polygon')
    .data(series)
    .enter()
    .append('polygon')
    .attrs(serie => {
      const points = serie
        .map((d, i) => {
          const votes = d[1] - d[0]
          const x = (1 - (votes / max) * Math.sin(i * angle)) * radius
          const y = (1 - (votes / max) * Math.cos(i * angle)) * radius

          return `${x},${y}`
        })
        .join(' ')

      return {
        fill: 'none',
        points,
      }
    })
    .styles((serie, i) => ({
      stroke: d3.schemePastel1[i],
      'stroke-width': 2,
    }))

  areaG
    .selectAll('g')
    .data(series)
    .enter()
    .append('g')
    .attr('class', 'nodes')
    .each((serie, nodeIndex, nodes) => {
      d3.select(nodes[nodeIndex])
        .selectAll('circle')
        .data(serie => serie)
        .enter()
        .append('circle')
        .attrs((d, i) => {
          const votes = d[1] - d[0]
          const x = (1 - (votes / max) * Math.sin(i * angle)) * radius
          const y = (1 - (votes / max) * Math.cos(i * angle)) * radius

          return {
            cx: x,
            cy: y,
            fill: d3.schemePastel1[nodeIndex],
            r: 4,
          }
        })
        .append('title')
        .text(d => `${serie.key}: ${formatVotes(d[1] - d[0])}`)
    })

  // Data source
  svg
    .append('text')
    .text('Source: https://elezionistorico.interno.gov.it/')
    .attr('transform', `translate(${-margin.left}, ${h + margin.bottom})`)
    .styles({
      'font-size': 10,
    })
})()
