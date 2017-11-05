import Foundation
import Vapor
import FluentProvider

/// # Category
/// The user can sort every item/product he creates into a category
final class Category: Model {
    var storage = Storage()
    
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
    static func prepare(_ database: Database) throws {
        try database.create(self, closure: { builder in
            builder.id()
            builder.string(Category.Keys.name)
        })
    }
    
    static func revert(_ database: Database) throws {
        try database.delete(self)
    }
}

//MARK: Node
extension Category: NodeRepresentable {
    func makeNode(in context: Context?) throws -> Node {
        var node = Node(context)
        try node.set(Category.Keys.id, id)
        try node.set(Category.Keys.name, name)
        return node
    }
}

//MARK: JSON
extension Category: JSONConvertible {
    convenience init(json: JSON) throws {
        try self.init(name: json.get(Category.Keys.name))
    }
    
    func makeJSON() throws -> JSON {
        var json = JSON()
        try json.set(Category.Keys.id, id)
        try json.set(Category.Keys.name, name)
        return json
    }
}

//MARK: Response
extension Category: ResponseRepresentable {}
