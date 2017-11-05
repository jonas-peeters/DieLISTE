@_exported import Vapor

extension Droplet {
    public func setup() throws {
        try setupRoutes()
        
        let users = UserController()
        users.addRoutes(drop: self)
    }
}
