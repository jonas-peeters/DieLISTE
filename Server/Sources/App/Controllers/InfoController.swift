import Vapor
import HTTP
import PostgreSQL

final class InfoController {
    
    /// Adds all routes relevant to special information
    ///
    /// - parameters:
    ///   - drop: The droplet to append the routes
    func addRoutes(drop: Droplet) {
        let info = drop.grouped("info")
        info.get("dbversion") {req in
            let postgresqlDriver = try drop.postgresql()
            return try makeJSON(from: postgresqlDriver.raw("SELECT version()"))
        }
        
        info.get("request") { req in
            return req.description
        }
    }
}
