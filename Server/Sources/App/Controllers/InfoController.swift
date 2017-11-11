import Vapor
import HTTP
import PostgreSQL

final class InfoController {
    
    func addRoutes(drop: Droplet) {
        let info = drop.grouped("info")
        info.get("dbversion") {req in
            let postgresqlDriver = try drop.postgresql()
            return try makeJSON(from: postgresqlDriver.raw("SELECT @@Version"))
        }
        
        info.get("request") { req in
            return req.description
        }
    }
}
