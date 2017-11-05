import Foundation
import Vapor
import FluentProvider

/// # Supermarket
///
/// Used to sort items in a list based on the users habit in that specific supermarket.
final class Supermarket: Model {
    var storage = Storage()
    
    var name: String
    var userId: Identifier?
    
    /// Keys for the column names in the database
    struct Keys {
        static let id = "id"
        static let name = "name"
        static let userId = "userId"
    }
    
    /// Creating a row for the databse from an item
    ///
    /// This function is only used by vapor internally
    func makeRow() throws -> Row {
        var row = Row()
        try row.set(Supermarket.Keys.name, name)
        try row.set(User.foreignIdKey, userId)
        return row
    }
    
    /// Creating an item from a row in the database
    ///
    /// This function is only used by vapor internally
    init(row: Row) throws {
        name = try row.get(Supermarket.Keys.name)
        userId = try row.get(User.foreignIdKey)
    }
    
    /// Creating a new supermarket instance
    ///
    /// This function is used for JSONRepresentable which makes routing etc. way easier
    ///
    /// - parameters:
    ///   - name: The name of the supermarket
    init(name: String, user: User) {
        self.name = name
        self.userId = user.id
    }
}

//MARK: Preparation
extension Supermarket: Preparation {
    static func prepare(_ database: Database) throws {
        try database.create(self, closure: { builder in
            builder.id()
            builder.string(Supermarket.Keys.name)
            builder.parent(User.self)
        })
    }
    
    static func revert(_ database: Database) throws {
        try database.delete(self)
    }
}

//MARK: Node
extension Supermarket: NodeRepresentable {
    func makeNode(in context: Context?) throws -> Node {
        var node = Node(context)
        try node.set(Supermarket.Keys.id, id)
        try node.set(Supermarket.Keys.name, name)
        try node.set(User.foreignIdKey, userId)
        return node
    }
}

//MARK: JSON
extension Supermarket: JSONConvertible {
    convenience init(json: JSON) throws {
        let userId: Identifier = try json.get(User.foreignIdKey)
        guard let user = try User.find(userId) else {
            throw Abort.badRequest
        }
        try self.init(name: json.get(Supermarket.Keys.name),
                      user: user)
    }
    
    func makeJSON() throws -> JSON {
        var json = JSON()
        try json.set(Supermarket.Keys.id, id)
        try json.set(Supermarket.Keys.name, name)
        try json.set(User.foreignIdKey, userId)
        return json
    }
}

//MARK: Response
extension Supermarket: ResponseRepresentable {}
