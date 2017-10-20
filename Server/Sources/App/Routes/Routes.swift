import Vapor
import MySQLProvider

extension Droplet {
    /// # Setting up Routes
    /// TODO
    func setupRoutes() throws {
        
        get("version") {req in
            var json = JSON()
            let mysqlDriver = try self.mysql()
            try json.set("version", mysqlDriver.raw("SELECT @@Version"))
            return json["version","@@Version"]!
        }
        
        get("user") {req in
            var json = JSON()
            let mysqlDriver = try self.mysql()
            try json.set("user", mysqlDriver.raw("SELECT * FROM user"))
            return json
        }
        
        get("vapor") { request in
            return Response(redirect: "http://vapor.codes")
        }
        
        get("hello") { req in
            var json = JSON()
            try json.set("hello", "world")
            return json
        }

        get("plaintext") { req in
            return "Hello, world!"
        }

        // response to requests to /info domain
        // with a description of the request
        get("info") { req in
            
            
            return req.description
        }

        get("description") { req in return req.description }
        
        try resource("posts", PostController.self)
        
        
        
    }
}
