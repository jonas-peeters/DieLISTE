import Vapor
import FluentProvider

/// # List
///
/// A list is the main part of the programm. It has a number of items as childs.
final class List: Model {
    /// Storage for internal vapor usage
    var storage = Storage()
    
    /// The name of the list
    var name: String
    
    /// Get the items of the list
    var items: [Item] {
        do {
            return try children(type: Item.self, foreignIdKey: List.foreignIdKey).all()
        } catch {
            return []
        }
    }
    
    var connectedUsers: Siblings<List, User, Pivot<User, List>> {
        return siblings()
    }
    
    /// Get the users that have access to the list
    var users: [String] {
        do {
            var userStringList: [String] = []
            for userModel in try connectedUsers.all() {
                userStringList.append(userModel.username)
            }
            return userStringList
        } catch {
            return []
        }
    }
    
    /// Keys for the column names in the database
    struct Keys {
        static let id = "id"
        static let name = "name"
    }
    
    /// Creating a row for the databse from an item
    ///
    /// This function is only used by vapor internally
    /// - throws: When the row can't be created possibly because of a issue when setting the keys and values
    /// - returns: A row containing all the information about the list
    func makeRow() throws -> Row {
        var row = Row()
        try row.set(List.Keys.name, name)
        return row
    }
    
    /// Creating an item from a row in the database
    ///
    /// This function is only used by vapor internally
    /// - parameters:
    ///   - row: A row from the database
    /// - throws: When a key can't be read from the database
    /// - returns: The list
    init(row: Row) throws {
        name = try row.get(List.Keys.name)
    }
    
    /// Creating a new list instance
    ///
    /// This function is used for JSONRepresentable which makes routing etc. way easier
    ///
    /// - parameters:
    ///   - name: The name of the list
    /// - returns: The list
    init(name: String) {
        self.name = name
    }
}

//MARK: Preparation
extension List: Preparation {
    /// Prepares the database for using the list model
    /// - parameters:
    ///   - database: The database that should be prepared
    /// - throws: When the database can't be created
    static func prepare(_ database: Database) throws {
        try database.create(self, closure: { builder in
            builder.id()
            builder.string(List.Keys.name)
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
extension List: NodeRepresentable {
    /// Makes the list model node representable
    ///
    /// - parameters:
    ///   - context: The context for the node encoding
    /// - throws: Throws if there is an error when setting the keys or values
    /// - returns: The list as a node
    func makeNode(in context: Context?) throws -> Node {
        var node = Node(context)
        try node.set(List.Keys.id, id)
        try node.set(List.Keys.name, name)
        return node
    }
}

//MARK: JSON
extension List: JSONConvertible {
    /// Creates a list from a JSON object
    ///
    /// Required json structure:
    ///
    ///     {
    ///       "name": $NAME_String,
    ///     }
    ///
    /// - parameters:
    ///   - json: A list encoded as JSON
    /// - throws: Throws if the json can't be parsed as a list
    /// - returns: The list
    convenience init(json: JSON) throws {
        try self.init(name: json.get(List.Keys.name))
    }
    
    /// Creates a JSON object from a list
    ///
    /// - throws: If something goes wrong when setting keys and values
    /// - returns: The list encoded as JSON
    func makeJSON() throws -> JSON {
        var json = JSON()
        try json.set(List.Keys.id, id)
        try json.set(List.Keys.name, name)
        return json
    }
}

//MARK: Response
extension List: ResponseRepresentable {}
