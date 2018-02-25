import Vapor
import FluentProvider

/// # Item or Product
///
/// An item is something you can buy.
/// As parent it has a list
final class Item: Model {
    /// Storage for internal vapor usage
    var storage = Storage()
    
    /// The name of the item
    var name: String
    /// The quantity of the item
    var quantity: String
    /// If the item is marked as completed
    var done: Bool = false
    /// The id of the list this item is in
    var listId: Identifier?
    /// The if of the category this item is in
    var categoryId: Identifier?
    
    /// Keys for the column names in the database
    struct Keys {
        static let id = "id"
        static let name = "name"
        static let quantity = "quantity"
        static let done = "done"
        static let listId = "listId"
        static let categoryId = "categoryId"
    }
    
    /// Creating a row for the databse from an item
    ///
    /// This function is only used by vapor internally
    /// - throws: When the row can't be created possibly because of a issue when setting the keys and values
    /// - returns: A row containing all the information about the item
    func makeRow() throws -> Row {
        var row = Row()
        try row.set(Item.Keys.name, name)
        try row.set(Item.Keys.quantity, quantity)
        try row.set(Item.Keys.done, done)
        try row.set(List.foreignIdKey, listId)
        try row.set(Category.foreignIdKey, categoryId)
        return row
    }
    
    /// Creating an item from a row in the database
    ///
    /// This function is only used by vapor internally
    /// - parameters:
    ///   - row: A row from the database
    /// - throws: When a key can't be read from the database
    /// - returns: The item
    init(row: Row) throws {
        name = try row.get(Item.Keys.name)
        quantity = try row.get(Item.Keys.quantity)
        done = try row.get(Item.Keys.done)
        listId = try row.get(List.foreignIdKey)
        categoryId = try row.get(Category.foreignIdKey)
    }
    
    /// Creating a new item instance
    ///
    /// This function is used for JSONRepresentable which makes routing etc. way easier
    ///
    /// - parameters:
    ///   - name: The name of the product
    ///   - quantity: How much of the product shall be bought. This is a string to allow different units
    ///   - done: If the item has been bought. (Won't be shown after a while in the app but is still saved for better auto completion etc.)
    ///   - list: The list the item is in
    ///   - category: The category the item is in
    init(name: String, quantity: String, done: Bool, list: Int, category: Int) {
        self.name = name
        self.quantity = quantity
        self.done = done
        self.listId = Identifier(list)
        self.categoryId = Identifier(category)
    }
}

//MARK: Preparation
extension Item: Preparation {
    /// Prepares the database for using the item model
    /// - parameters:
    ///   - database: The database that should be prepared
    /// - throws: When the database can't be created
    static func prepare(_ database: Database) throws {
        try database.create(self, closure: { builder in
            builder.id()
            builder.string(Item.Keys.name)
            builder.string(Item.Keys.quantity)
            builder.bool(Item.Keys.done)
            builder.parent(List.self)
            builder.parent(Category.self)
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
extension Item: NodeRepresentable {
    /// Makes the item model node representable
    ///
    /// - parameters:
    ///   - context: The context for the node encoding
    /// - throws: Throws if there is an error when setting the keys or values
    /// - returns: The item as a node
    func makeNode(in context: Context?) throws -> Node {
        var node = Node(context)
        try node.set(Item.Keys.id, id)
        try node.set(Item.Keys.name, name)
        try node.set(Item.Keys.quantity, quantity)
        try node.set(Item.Keys.done, done)
        try node.set(List.foreignIdKey, listId)
        try node.set(Category.foreignIdKey, categoryId)
        return node
    }
}

//MARK: JSON
extension Item: JSONConvertible {
    /// Creates an item from a JSON object
    ///
    /// Required json structure:
    ///
    ///     {
    ///       "name": $NAME_String,
    ///       "quantity": $QUANTITY_String,
    ///       "done": $DONE_Boolean,
    ///       "listId": $LISTID_Int,
    ///       "categoryId": $CATEGORYID_Int
    ///     }
    ///
    /// - parameters:
    ///   - json: An item encoded as JSON
    /// - throws: Throws if the json can't be parsed as an item
    /// - returns: The item
    convenience init(json: JSON) throws {
        try self.init(name: json.get(Item.Keys.name),
                      quantity: json.get(Item.Keys.quantity),
                      done: json.get(Item.Keys.done),
                      list: json.get(Item.Keys.listId),
                      category: json.get(Item.Keys.categoryId))
    }
    
    /// Creates a JSON object from an item
    ///
    /// - throws: If something goes wrong when setting keys and values
    /// - returns: The item encoded as JSON
    func makeJSON() throws -> JSON {
        var json = JSON()
        try json.set(Item.Keys.id, id)
        try json.set(Item.Keys.name, name)
        try json.set(Item.Keys.quantity, quantity)
        try json.set(Item.Keys.done, done)
        try json.set(Item.Keys.categoryId, categoryId)
        return json
    }
}

//MARK: Response
extension Item: ResponseRepresentable {}
