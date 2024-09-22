const RTC_CONFIG = {
    'iceServers': [
        { 'urls': 'stun:stun.stunprotocol.org:3478' },
        { 'urls': 'stun:stun.l.google.com:19302' },
    ]
}

let uuid
let dataChannel
let serverConnection
let peerConn
async function pageReady() {
    uuid = createUUID()
    serverConnection = new WebSocket(`ws://${window.location.hostname}:8443`)
    console.log(uuid)
    serverConnection.onmessage = function (message) {
        if (!peerConn) {
            peerConn = new RTCPeerConnection(RTC_CONFIG)
            peerConn.onicecandidate = function (event) {
                if (event.candidate != null) {
                    serverConnection.send(JSON.stringify({ 'ice': event.candidate, 'uuid': uuid }))
                }
            }
            peerConn.ondatachannel = function(event) {
                dataChannel = event.channel
                dataChannel.onmessage = function(event) {
                    console.log('datachannel', event)
                }
            }
        }
        const signal = JSON.parse(message.data)
        if (signal.uuid == uuid) {
            return
        }
        console.log('recieved message')
        console.log(message)
        if (signal.sdp) {
            console.log('setting remote description')
            peerConn.setRemoteDescription(new RTCSessionDescription(signal.sdp)).then(() => {
                // Only create answers in response to offers
                if (signal.sdp.type !== 'offer') return;
                console.log('answering....')
                peerConn.createAnswer().then(description => peerConn.setLocalDescription(description).then(() => {
                    serverConnection.send(JSON.stringify({ 'sdp': peerConn.localDescription, 'uuid': uuid }))
                }).catch(handleError)).catch(handleError)
            }).catch(handleError)
        } else if (signal.ice) {
            peerConn.addIceCandidate(new RTCIceCandidate(signal.ice)).catch(handleError)
        }
        peerConn.onconnectionstatechange = event => console.log(event)
    }
}

// Taken from http://stackoverflow.com/a/105074/515584
// Strictly speaking, it's not a real UUID, but it gets the job done here
function createUUID() {
    function s4() {
        return Math.floor((1 + Math.random()) * 0x10000).toString(16).substring(1);
    }

    return `${s4() + s4()}-${s4()}-${s4()}-${s4()}-${s4() + s4() + s4()}`;
}

function handleError(e) {
    console.log(e)
}

function start() {
    peerConn = new RTCPeerConnection(RTC_CONFIG)
    dataChannel = peerConn.createDataChannel('data')
    dataChannel.onmessage = function(event) {
        console.log('datachannel', event)
    }
    peerConn.onicecandidate = function (event) {
        if (event.candidate != null) {
            serverConnection.send(JSON.stringify({ 'ice': event.candidate, 'uuid': uuid }));
        }
    }
    console.log('offering....')
    peerConn.createOffer().then(description => peerConn.setLocalDescription(description).then(() => {
        serverConnection.send(JSON.stringify({ 'sdp': peerConn.localDescription, 'uuid': uuid }))
    }).catch(handleError)).catch(handleError)
}

function send() {
    dataChannel.send('hello world')
}