@_exported import Vapor

extension Droplet {
    /// Setting up all routes
    public func setup() throws {
        try setupRoutes()
    }
}
