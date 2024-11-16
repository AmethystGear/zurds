let playerId
let peerConn
let dataChannel
let eventSource
let recievedAllIceCandidates = false
let sentAllIceCandidates = false

const RTC_CONFIG = {
    "iceServers": [
        { "urls": "stun:stun.stunprotocol.org:3478" },
        { "urls": "stun:stun.l.google.com:19302" }
    ]
}

function setupEventSource(onPlayerId) {
    console.log('setting up event source')
    eventSource = new EventSource('/player')

    eventSource.addEventListener('ping', () => console.log('server ping'))

    eventSource.addEventListener('playerId', async function (event) {
        playerId = JSON.parse(event.data)
        console.log('set player id to:', playerId)
        await onPlayerId()
    })

    eventSource.addEventListener('msg', async function (message) {
        console.log('recieved message from server', message)
        if (peerConn === undefined) {
            peerConn = new RTCPeerConnection(RTC_CONFIG)
            peerConn.onicecandidate = onIceCandidate
            peerConn.ondatachannel = function (event) {
                dataChannel = event.channel
                dataChannel.onmessage = onMessage
            }
        }
        let signal = JSON.parse(message.data)
        if ('sdp' in signal) {
            console.log('setting remote description')
            peerConn.setRemoteDescription(new RTCSessionDescription(signal.sdp)).then(() => {
                if (signal.sdp.type !== 'offer') {
                    return
                }
                peerConn.createAnswer().then(description =>
                    peerConn.setLocalDescription(description).then(sendLocalDescription).catch(handleError))
                    .catch(handleError)
            }).catch(handleError)
        } else if ('ice' in signal) {
            peerConn.addIceCandidate(new RTCIceCandidate(signal.ice)).catch(handleError)
            if (signal.ice.candidate === '') {
                recievedAllIceCandidates = true
                if (sentAllIceCandidates) {
                    closeEventSource()
                }
            }
        }
        peerConn.onconnectionstatechange = event => console.log(event)
    })
}

function closeEventSource() {
    console.log('closing event source')
    eventSource.close()
    eventSource = undefined
}

async function vend() {
    withPlayerId(async () => {
        let response = await fetch('/vend', {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json'
            },
            body: JSON.stringify({ 'player_id': playerId })
        })
        let body = await response.json()
        if (response.ok) {
            if (peerConn !== undefined) {
                peerConn.close()
                sentAllIceCandidates = false
                recievedAllIceCandidates = false
                peerConn = undefined
            }
            document.querySelector('.token-text').textContent = body['token']
        } else {
            alert(body['err'])
        }
    })
}

async function accept() {
    withPlayerId(async () => {
        let token = document.getElementById('tokenInput').value
        let response = await fetch('/accept', {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json'
            },
            body: JSON.stringify({ 'player_id': playerId, 'token': token })
        })
        if (response.ok) {
            if (peerConn !== undefined) {
                peerConn.close()
                sentAllIceCandidates = false
                recievedAllIceCandidates = false
            }
            peerConn = new RTCPeerConnection(RTC_CONFIG)
            dataChannel = peerConn.createDataChannel('data')
            dataChannel.onmessage = onMessage
            peerConn.onicecandidate = onIceCandidate
            console.log('offering....')
            peerConn.createOffer().then(description =>
                peerConn.setLocalDescription(description).then(sendLocalDescription).catch(handleError))
                .catch(handleError)

            dataChannel.onopen = function (event) {
                dataChannel.send('{"challenge" : "accepted"}')
            }
        } else {
            let body = await response.json()
            alert(body['err'])
        }
    })
}

async function onIceCandidate(event) {
    console.log(event.candidate)
    if (event.candidate !== null) {
        await fetch('/msg', {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json'
            },
            body: JSON.stringify({ 'player_id': playerId, 'content': { 'ice': event.candidate } })
        })
    } else {
        sentAllIceCandidates = true
        if (recievedAllIceCandidates) {
            closeEventSource()
        }
    }
}

async function sendLocalDescription() {
    await fetch('/msg', {
        method: 'POST',
        headers: {
            'Content-Type': 'application/json'
        },
        body: JSON.stringify({ 'player_id': playerId, 'content': { 'sdp': peerConn.localDescription } })
    })
}

function onMessage(message) {
    console.log('recieved message from player', message)
    let data = JSON.parse(message.data)
    if (data['challenge'] === 'accepted') {
        dataChannel.send('{"challenge" : "nah"}')
    }
}

function handleError(e) {
    console.log(e)
}

async function withPlayerId(fn) {
    if (eventSource === undefined) {
        setupEventSource(fn)
    } else {
        await fn()
    }
}

document.addEventListener('mousemove', (event) => {
    let leftSection = document.querySelector('.left-section')
    let rightSection = document.querySelector('.right-section')
    let x = rightSection.matches(':hover') ? 30 : 45
    leftSection.style.transition = 'clip-path 0.25s ease-in-out'
    leftSection.style.clipPath = `polygon(0 0, ${x + 20}% 0, ${x}% 100%, 0% 100%)`
})
document.querySelector('.challenge-text').addEventListener('click', vend)
document.getElementById('acceptButton').addEventListener('click', accept)