const rtcConfig = {
    'iceServers': [
        { 'urls': 'stun:stun.stunprotocol.org:3478' },
        { 'urls': 'stun:stun.l.google.com:19302' },
    ]
}

async function evt(elemId, event, filter) {
    return new Promise(function (resolve) {
        document.getElementById(elemId).addEventListener(event, e => {
            if (filter === undefined || filter(e)) {
                this.removeEventListener(event, arguments.callee, false)
                resolve(e)
            }
        })
    })
}


let keyEvent = await evt('player-mame', 'keydown', e => e.key === 'Enter')
let playerName = keyEvent.target.value
validateName(playerName)
let sse = new SSE(new EventSource('/join/' + playerName))
let playerId = await sse.get('player-id')
let ev = await Promise.race([
    evt('find-random-match', 'onclick'), 
    evt('send-join-request', 'onclick'), 
    evt('cancel', 'onclick'), 
    sse.get('join-request')])

console.log(ev)


if (ev.target.id === 'find-random-match') {
    await fetch('/find-pair/' + playerId, { method: 'POST' })
}





document.getElementById('playerName').addEventListener('keydown', async event => {
    if (event.key === 'Enter') {
        let name = event.target.value
        validateName(name)
        let sse = new SSE(new EventSource('/join/' + name))
        let playerId = await sse.get('player-id')
        await evt('findMatch', 'onclick')


    }
})

function join(playerId, sse) {
    document.getElementById('findMatch').addEventListener('onclick', () => {
        fetch('/find-pair/' + playerId).then(() => {
            sse.listen('')
        })
    })
}

function validateName(name) {
    function invalidName(msg) {
        throw new Error('invalid player name: ' + msg)
    }
    if (name.length == 0) {
        invalidName('empty')
    }
    if (name.length > 40) {
        invalidName('keep length under 40 characters')
    }
    if (!name.match(/^[a-z0-9_-]+$/i)) {
        invalidName('use alphanumeric characters, underscores, or dashes')
    }
}

class SSE {
    constructor(sse) {
        this.sse = sse
    }

    async get(event) {
        return new Promise(function (resolve) {
            this.sse.addEventListener(event, e => {
                this.removeEventListener(event, arguments.callee, false)
                resolve(JSON.parse(e.data))
            })
        })
    }

    listen(event, callback) {
        this.sse.addEventListener(event, e => callback(JSON.parse(e.data)))
    }
}

