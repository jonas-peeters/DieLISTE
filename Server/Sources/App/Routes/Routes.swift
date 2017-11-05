import Vapor
import MySQLProvider

extension Droplet {
    /// # Setting up Routes
    /// TODO
    func setupRoutes() throws {
        get("version") {req in
            let mysqlDriver = try self.mysql()
            return try makeJSON(from: mysqlDriver.raw("SELECT @@Version"))
        }

        get("info") { req in
            return req.description
        }
        
        post("user", handler: UserController.create(UserController()))
        
    }
}
