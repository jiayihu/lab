const fs = require('fs')

const file = fs.readFileSync('./voti.csv', 'utf-8')
const [header, ...rows] = file.split('\n')
const formatted = rows.reduce((acc, row) => {
  const [region, list, votes] = row.split(',')

  if (!region) return acc

  const normalizedRegion = region.match(/\w+/)[0].toLowerCase()
  const normalizedList = list.toLowerCase()
  const normalizedVotes = Number(votes)
  const existing = acc.find(x => x.region === normalizedRegion && x.list === normalizedList)

  if (existing) existing.votes += normalizedVotes
  else acc.push({ region: normalizedRegion, list: normalizedList, votes: normalizedVotes })

  return acc
}, [])
fs.writeFileSync('./votes.csv', formatted.map(x => `${x.region},${x.list},${x.votes}`).join('\n'))
