import Vapor
import HTTP
import MySQL

final class InfoController {
    
    func addRoutes(drop: Droplet) {
        let info = drop.grouped("info")
        info.get("dbversion") {req in
            let mysqlDriver = try drop.mysql()
            return try makeJSON(from: mysqlDriver.raw("SELECT @@Version"))
        }
        
        info.get("request") { req in
            return req.description
        }
    }
}
