const fs = require('fs')
const util = require('util')
const readFile = util.promisify(fs.readFile)

const setOutput = (wires, output, eval) => {
  if (!wires.has(output)) {
    const maybeValue = eval()
    maybeValue && wires.set(output, maybeValue.value)
  }
}

const tryGetValue = (wires, input) => {
  const n = Number(input)
  if (Number.isFinite(n)) return [true, n]
  if (wires.has(input)) return [true, wires.get(input)]
  return [false, undefined]
}

const zeroInputGate = (wires, value, output) =>
  setOutput(wires, output, () => ({ value }))

const oneInputGate = (wires, input, output, f) =>
  setOutput(wires, output, () => {
    const [gotValue, value] = tryGetValue(wires, input)
    return gotValue && { value: f(value) }
  })

const twoInputGate = (wires, input1, input2, output, f) =>
  setOutput(wires, output, () => {
    const [gotValue1, value1] = tryGetValue(wires, input1)
    const [gotValue2, value2] = tryGetValue(wires, input2)
    return gotValue1 && gotValue2 && { value: f(value1, value2) }
  })

const parseLine = line => {
  const parsings = [
    [/^(\d+) -> ([a-z]+)/, m => wires =>
      zeroInputGate(wires, Number(m[1]), m[2])],
    [/^([a-z]+) -> ([a-z]+)/, m => wires =>
      oneInputGate(wires, m[1], m[2], v => v)],
    [/^NOT (\d+|[a-z]+) -> ([a-z]+)/, m => wires =>
      oneInputGate(wires, m[1], m[2], v => ~v & 0xFFFF)],
    [/^(\d+|[a-z]+) LSHIFT (\d+) -> ([a-z]+)/, m => wires =>
      oneInputGate(wires, m[1], m[3], v => v << m[2])],
    [/^(\d+|[a-z]+) RSHIFT (\d+) -> ([a-z]+)/, m => wires =>
      oneInputGate(wires, m[1], m[3], v => v >> m[2])],
    [/^(\d+|[a-z]+) AND (\d+|[a-z]+) -> ([a-z]+)/, m => wires =>
      twoInputGate(wires, m[1], m[2], m[3], (v1, v2) => v1 & v2)],
    [/^(\d+|[a-z]+) OR (\d+|[a-z]+) -> ([a-z]+)/, m => wires =>
      twoInputGate(wires, m[1], m[2], m[3], (v1, v2) => v1 | v2)],
  ]
  for ([r, f] of parsings) {
    const match = r.exec(line)
    if (match) return f(match)
  }
  throw new Error(`Failed to parse line, "${line}".`)
}

const parseLines = s =>
  s.split('\n').filter(s => s.length).map(parseLine)

const run = (instructions, ...overrides) => {
  const wires = new Map(overrides)
  const loop = () => {
    if (wires.has('a')) return wires.get('a')
    instructions.forEach(instruction => instruction(wires))
    return loop()
  }
  return loop()
}

const evalCircuit = async path => {
  try {
    const buffer = await readFile(path)
    const instructions = parseLines(buffer.toString())
    const a = run(instructions)
    console.log(`[part 1 answer] ${a}`)
    console.log(`[part 2 answer] ${run(instructions, ['b', a])}`)
  } catch (error) {
    console.log(`readFile error: ${error}`)
  }
}

evalCircuit('Day07/src/main/resources/input.txt')
