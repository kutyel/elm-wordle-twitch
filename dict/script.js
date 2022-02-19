const xmlParser = require('xml2json')
const { readFileSync, writeFileSync } = require('fs')

const xml = readFileSync('./JMDict', 'utf8')
const db = JSON.parse(xmlParser.toJson(xml))

// Helper function to determine if all letters in a word are katakana
// Unicode taken from https://en.wikipedia.org/wiki/Katakana_(Unicode_block)
const allKatakana = (word) => !!word && /[\u30A1-\u30EF]/.test(word)

// Find all the words containing only katakana
const allKatakanaWords = db.JMdict.entry
  .map(({ r_ele }) => r_ele.reb)
  .filter(allKatakana)

const all5LetterKatakanaWords = allKatakanaWords.filter(
  ({ length }) => length == 5
)

writeFileSync(
  './katakana.json',
  JSON.stringify(all5LetterKatakanaWords, null, 4)
)
