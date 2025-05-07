## Goals
### Key Goals
- Local server should be http as to not require user to setup tls certificates
- Client should be able to get a list of available servers
- Client and local server should be able to communicate securely without https
    - Public key encryption
    - Should store keys in cookie

### Other Goals
- Remote server should protect servers

## Structure
- Client
    - Front end interface
- Remote server
    - The central server that mediates connections between clients and local servers
    - Remote server is secure
- Local server
    - A server run by individuals to securely host their files

## Implementation
- clientKey := Client public key
- localServerKey := Local server public key
- Client
    - Use svelte because it looks interesting
    + generateKey() -> public, private key
    + encrypt()
    + decrypt()
- api
    + validatePassword()
- Remote server
    + servers
        - Store servers in memory
        - If you store it in database/file you can re-connect servers on restart
    + getServers(name) -> servers
    + addServer(id, name, password)
    + connectClientToServer(id, clientKey, password) -> server
- Local server
    + addServer()
        - Should run every couple of minutes
    + connectToClient(clientKey, password) -> publicKey
        - Validate client
    + encrypt()
    + decrypt()
    ...
        - All methods handling zettelkasten should be protected
- Test
    - Can create and read created zettel
    - Encrypt/decrypt works
    - Can add server
    - Client can connect
