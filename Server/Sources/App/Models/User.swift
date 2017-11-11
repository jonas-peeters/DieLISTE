import Foundation
import Vapor
import FluentProvider
import AuthProvider

/// # User
///
/// This is a model of all the properties a user has
public final class User: Model, SessionPersistable, PasswordAuthenticatable {
    public let storage = Storage()
    
    var username: String
    var email: String
    var verifiedEmail: Bool
    var password: String
    var allergies: String
    var spamCounter: Int
    
    var lists: Siblings<User, List, Pivot<User, List>> {
        return siblings()
    }
    
    /// # Keys
    /// This struct defines a number of keys that are
    /// used in the database to name the single columns
    struct Keys {
        static let id = "id"
        static let username = "username"
        static let email = "email"
        static let verifiedEmail = "verified"
        static let password = "password"
        static let allergies = "allergies"
        static let spam = "spamCounter"
    }
    
    /// Creates a User by the given parameters
    ///
    /// - parameters:
    ///   - username: The username
    ///   - email: The users email
    ///   - verifiedEmail: If the users email is verified
    ///   - password: a hash from the users password with a bit of salt
    ///   - allergies: a string containing allergies the user has
    ///   - spamCounter: The number of times spam was reported for this user
    public init(username: String, email: String, verifiedEmail: Bool, password: String, allergies: String, spamCounter: Int) {
        self.username = username
        self.email = email
        self.verifiedEmail = verifiedEmail
        self.password = password
        self.allergies = allergies
        self.spamCounter = spamCounter
    }
    
    /// Creates a row in the database from a User
    public func makeRow() throws -> Row {
        var row = Row()
        try row.set(User.Keys.username, username)
        try row.set(User.Keys.email, email)
        try row.set(User.Keys.verifiedEmail, verifiedEmail)
        try row.set(User.Keys.password, password)
        try row.set(User.Keys.allergies, allergies)
        try row.set(User.Keys.spam, spamCounter)
        return row
    }
    
    /// Creates a User from a row in the database
    public init(row: Row) throws {
        username = try row.get(User.Keys.username)
        email = try row.get(User.Keys.email)
        verifiedEmail = try row.get(User.Keys.verifiedEmail)
        password = try row.get(User.Keys.password)
        allergies = try row.get(User.Keys.allergies)
        spamCounter = try row.get(User.Keys.spam)
    }
    
    public func getLists()  {
        
    }
}


// MARK: Fluent Preparation
extension User: Preparation {
    /// Prepares the database for using the user model
    public static func prepare(_ database: Database) throws {
        try database.create(self, closure: { builder in
            builder.id()
            builder.string(User.Keys.username)
            builder.string(User.Keys.email)
            builder.bool(User.Keys.verifiedEmail)
            builder.string(User.Keys.password)
            builder.string(User.Keys.allergies)
            builder.string(User.Keys.spam)
        })
    }
    
    /// Undoes what prepare() ist doing
    public static func revert(_ database: Database) throws {
        try database.delete(self)
    }
}


// MARK: Node
extension User: NodeRepresentable {
    /// Makes the user model node representable
    public func makeNode(in context: Context?) throws -> Node {
        var node = Node(context)
        try node.set(User.Keys.id, id)
        try node.set(User.Keys.username, username)
        try node.set(User.Keys.email, email)
        try node.set(User.Keys.verifiedEmail, verifiedEmail)
        try node.set(User.Keys.password, password)
        try node.set(User.Keys.allergies, allergies)
        try node.set(User.Keys.spam, spamCounter)
        return node
    }
}

//MARK: JSON
extension User: JSONConvertible {
    public convenience init(json: JSON) throws {
        try self.init(username: json.get(User.Keys.username),
                      email: json.get(User.Keys.email),
                      verifiedEmail: false,
                      password: json.get(User.Keys.password),
                      allergies: "",
                      spamCounter: 0)
    }
    
    public func makeJSON() throws -> JSON {
        var json = JSON()
        try json.set(User.Keys.id, id)
        try json.set(User.Keys.username, username)
        try json.set(User.Keys.email, email)
        try json.set(User.Keys.verifiedEmail, verifiedEmail)
        try json.set(User.Keys.password, password)
        try json.set(User.Keys.allergies, allergies)
        try json.set(User.Keys.spam, spamCounter)
        return json
    }
}

//MARK: Response
extension User: ResponseRepresentable {}
