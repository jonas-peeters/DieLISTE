import Foundation
import Vapor
import FluentProvider

/// # List
///
/// A list is the main part of the programm. It has a number of items as childs.
final class List: Model {
    var storage = Storage()
    
    var name: String
    
    var items: Siblings<List, Item, Pivot<List, Item>> {
        return siblings()
    }
    
    struct Keys {
        static let id = "id"
        static let name = "name"
    }
    
    /// Creating a row for the databse from an item
    ///
    /// This function is only used by vapor internally
    func makeRow() throws -> Row {
        var row = Row()
        try row.set(List.Keys.name, name)
        return row
    }
    
    /// Creating an item from a row in the database
    ///
    /// This function is only used by vapor internally
    init(row: Row) throws {
        name = try row.get(List.Keys.name)
    }
    
    /// Creating a new list instance
    ///
    /// This function is used for JSONRepresentable which makes routing etc. way easier
    ///
    /// - parameters:
    ///   - name: The name of the list
    init(name: String) {
        self.name = name
    }
}

//MARK: Preparation
extension List: Preparation {
    static func prepare(_ database: Database) throws {
        try database.create(self, closure: { builder in
            builder.id()
            builder.string(List.Keys.name)
        })
    }
    
    static func revert(_ database: Database) throws {
        try database.delete(self)
    }
}

//MARK: Node
extension List: NodeRepresentable {
    func makeNode(in context: Context?) throws -> Node {
        var node = Node(context)
        try node.set(List.Keys.id, id)
        try node.set(List.Keys.name, name)
        return node
    }
}

//MARK: JSON
extension List: JSONConvertible {
    convenience init(json: JSON) throws {
        try self.init(name: json.get(List.Keys.name))
    }
    
    func makeJSON() throws -> JSON {
        var json = JSON()
        try json.set(List.Keys.id, id)
        try json.set(List.Keys.name, name)
        return json
    }
}

//MARK: Response
extension List: ResponseRepresentable {}
