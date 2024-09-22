const evt_source = new EventSource('/player')
evt_source.addEventListener('ping', function(event) {
    console.log('server ping')
})

let player_id
evt_source.addEventListener('player_id', function(event) {
    player_id = JSON.parse(event.data)
    console.log('set player id to:', player_id)
})

evt_source.addEventListener('msg', function(event) {

})

// Function to call the /vend API and display the token
async function vend() {
    if (!player_id) {
        console.error('Player ID not available')
        return
    }
    
    try {
        const response = await fetch('/vend', {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json'
            },
            body: JSON.stringify({ player_id })
        })

        if (!response.ok) {
            console.error('Vend failed')
            return
        }

        const data = await response.json()
        const token = data.token // Assuming the token is in the response JSON as { token: '...' }

        // Display the returned token in the page
        document.getElementById('vendToken').textContent = token
        console.log('Vend successful, token:', token)
    } catch (error) {
        console.error('Error during vend:', error)
    }
}

// Function to call the /accept API
async function accept() {
    const token = document.getElementById('tokenInput').value
    if (!player_id || !token) {
        console.error('Player ID or token not available')
        return
    }
    
    try {
        const response = await fetch('/accept', {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json'
            },
            body: JSON.stringify({ player_id, token })
        })

        if (!response.ok) {
            console.error('Accept failed')
        } else {
            console.log('Accept successful')
        }
    } catch (error) {
        console.error('Error during accept:', error)
    }
}

// Attach event listeners to buttons
document.getElementById('vendButton').addEventListener('click', vend)
document.getElementById('acceptButton').addEventListener('click', accept)