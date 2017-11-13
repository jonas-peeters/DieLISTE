import Vapor
import FluentProvider

/// # Supermarket
///
/// Used to sort items in a list based on the users habit in that specific supermarket.
final class Supermarket: Model {
    /// Storage for internal vapor usage
    var storage = Storage()
    
    /// The name of the supermarket
    var name: String
    /// The id of the user who is the parent of this supermarket
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
    ///
    /// - throws: When the row can't be created possibly because of a issue when setting the keys and values
    /// - returns: A row containing all the information about the user
    func makeRow() throws -> Row {
        var row = Row()
        try row.set(Supermarket.Keys.name, name)
        try row.set(User.foreignIdKey, userId)
        return row
    }
    
    /// Creating an item from a row in the database
    ///
    /// This function is only used by vapor internally
    /// - parameters:
    ///   - row: A row from the database
    /// - throws: When a key can't be read from the database
    /// - returns: The supermarket
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
    ///   - user: The user who is the parent of this supermarket model
    /// - returns: The supermarket
    init(name: String, user: User) {
        self.name = name
        self.userId = user.id
    }
}

//MARK: Preparation
extension Supermarket: Preparation {
    /// Prepares the database for using the supermarket model
    /// - parameters:
    ///   - database: The database that should be prepared
    /// - throws: When the database can't be reverted
    static func prepare(_ database: Database) throws {
        try database.create(self, closure: { builder in
            builder.id()
            builder.string(Supermarket.Keys.name)
            builder.parent(User.self)
        })
    }
    
    /// Undoes what prepare() ist doing
    /// - parameters:
    ///   - database: The database where the preparation should be reverted
    /// - throws: When the database can't be reverted
    static func revert(_ database: Database) throws {
        try database.delete(self)
    }
}

//MARK: Node
extension Supermarket: NodeRepresentable {
    /// Makes the supermarket model node representable
    ///
    /// - parameters:
    ///   - context: The context for the node encoding
    /// - throws: Throws if the is an error when setting the keys or values
    /// - returns: The supermarket as a node
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
    /// Creates a supermarket from a JSON object of the supermarket
    ///
    /// Required json structure:
    ///
    ///     {
    ///       "name": $NAME_String
    ///     }
    ///
    /// - parameters:
    ///   - json: A supermarket encoded as JSON
    /// - throws: Throws if the json can't be parsed as a supermarket
    /// - returns: The supermarket
    convenience init(json: JSON) throws {
        let userId: Identifier = try json.get(User.foreignIdKey)
        guard let user = try User.find(userId) else {
            throw Abort.badRequest
        }
        try self.init(name: json.get(Supermarket.Keys.name),
                      user: user)
    }
    
    /// Creates a JSON object from a supermarket
    ///
    /// - throws: If something goes wrong when setting keys and values
    /// - returns: The supermarket encoded as JSON
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
