import Vapor
import FluentProvider

/// # Category
/// The user can sort every item/product he creates into a category
final class Category: Model {
    /// Storage for internal vapor usage
    var storage = Storage()
    
    /// Category name
    var name: String
    
    /// Keys for the column names in the database
    struct Keys {
        static let id = "id"
        static let name = "name"
    }
    
    /// Creating a row for the databse from an item
    ///
    /// This function is only used by vapor internally
    func makeRow() throws -> Row {
        var row = Row()
        try row.set(Category.Keys.name, name)
        return row
    }
    
    /// Creating an item from a row in the database
    ///
    /// This function is only used by vapor internally
    init(row: Row) throws {
        name = try row.get(Category.Keys.name)
    }
    
    /// Creating a new category instance
    ///
    /// This function is used for JSONRepresentable which makes routing etc. way easier
    ///
    /// - parameters:
    ///   - name: The name of the category
    init(name: String) {
        self.name = name
    }
}

//MARK: Preparation
extension Category: Preparation {
    /// Prepares the database for using the category model and adding the categories
    ///
    /// - parameters:
    ///   - database: The database that should be prepared
    /// - throws: When the database can't be reverted
    static func prepare(_ database: Database) throws {
        try database.create(self, closure: { builder in
            builder.id()
            builder.string(Category.Keys.name)
        })
        try Category(name: "Fruits").save()
        try Category(name: "Vegtables").save()
        try Category(name: "Drinks").save()
        try Category(name: "Meat").save()
        try Category(name: "Sweets").save()
        try Category(name: "Other").save()
        try Category(name: "Frozen").save()
        try Category(name: "Cheese").save()
        try Category(name: "Spread").save()
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
extension Category: NodeRepresentable {
    /// Makes the category model node representable
    ///
    /// - parameters:
    ///   - context: The context for the node encoding
    /// - throws: Throws if there is an error when setting the keys or values
    /// - returns: The item as a node
    func makeNode(in context: Context?) throws -> Node {
        var node = Node(context)
        try node.set(Category.Keys.id, id)
        try node.set(Category.Keys.name, name)
        return node
    }
}

//MARK: JSON
extension Category: JSONConvertible {
    /// Creates a category from a JSON object
    ///
    /// Required json structure:
    ///
    ///     {
    ///       "name": $NAME_String
    ///     }
    ///
    /// - parameters:
    ///   - json: A categroy encoded as JSON
    /// - throws: Throws if the json can't be parsed as a category
    /// - returns: The category
    convenience init(json: JSON) throws {
        try self.init(name: json.get(Category.Keys.name))
    }
    
    /// Creates a JSON object from a category
    ///
    /// - throws: If something goes wrong when setting keys and values
    /// - returns: The category encoded as JSON
    func makeJSON() throws -> JSON {
        var json = JSON()
        try json.set(Category.Keys.id, id)
        try json.set(Category.Keys.name, name)
        return json
    }
}

//MARK: Response
extension Category: ResponseRepresentable {}
