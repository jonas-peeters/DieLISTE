import Foundation
import Vapor
import FluentProvider

/// # User
///
/// This is a model of all the properties a user has
final class User: Model {
    let storage = Storage()
    
    var username: String
    var email: String
    var verifiedEmail: Bool
    var passwordHash: String
    var allergies: String
    var spamCounter: Int
    
    /// # Keys
    /// This struct defines a number of keys that are
    /// used in the database to name the single columns
    struct Keys {
        static let id = "id"
        static let username = "username"
        static let email = "email"
        static let verifiedEmail = "verified"
        static let passwordHash = "passwordHash"
        static let allergies = "allergies"
        static let spam = "spamCounter"
    }
    
    /// Creates a User by the given parameters
    ///
    /// - parameters:
    ///   - username: The username
    ///   - email: The users email
    ///   - verifiedEmail: If the users email is verified
    ///   - passwordHash: a hash from the users password with a bit of salt
    ///   - allergies: a string containing allergies the user has
    ///   - spamCounter: The number of times spam was reported for this user
    init(username: String, email: String, verifiedEmail: Bool, passwordHash: String, allergies: String, spamCounter: Int) {
        self.username = username
        self.email = email
        self.verifiedEmail = verifiedEmail
        self.passwordHash = passwordHash
        self.allergies = allergies
        self.spamCounter = spamCounter
    }
    
    /// Creates a row in the database from a User
    func makeRow() throws -> Row {
        var row = Row()
        try row.set(User.Keys.username, username)
        try row.set(User.Keys.email, email)
        try row.set(User.Keys.verifiedEmail, verifiedEmail)
        try row.set(User.Keys.passwordHash, passwordHash)
        try row.set(User.Keys.allergies, allergies)
        try row.set(User.Keys.spam, spamCounter)
        return row
    }
    
    /// Creates a User from a row in the database
    init(row: Row) throws {
        username = try row.get(User.Keys.username)
        email = try row.get(User.Keys.email)
        verifiedEmail = try row.get(User.Keys.verifiedEmail)
        passwordHash = try row.get(User.Keys.passwordHash)
        allergies = try row.get(User.Keys.allergies)
        spamCounter = try row.get(User.Keys.spam)
    }
}


// MARK: Fluent Preparation
extension User: Preparation {
    /// Prepares the database for using the user model
    static func prepare(_ database: Database) throws {
        try database.create(self, closure: { builder in
            builder.id()
            builder.string(User.Keys.username)
            builder.string(User.Keys.email)
            builder.bool(User.Keys.verifiedEmail)
            builder.string(User.Keys.passwordHash)
            builder.string(User.Keys.allergies)
            builder.string(User.Keys.spam)
        })
    }
    
    /// Undoes what prepare() ist doing
    static func revert(_ database: Database) throws {
        try database.delete(self)
    }
}


// MARK: Node
extension User: NodeRepresentable {
    /// Makes the user model node representable
    func makeNode(in context: Context?) throws -> Node {
        var node = Node(context)
        try node.set(User.Keys.id, id)
        try node.set(User.Keys.username, username)
        try node.set(User.Keys.email, email)
        try node.set(User.Keys.verifiedEmail, verifiedEmail)
        try node.set(User.Keys.passwordHash, passwordHash)
        try node.set(User.Keys.allergies, allergies)
        try node.set(User.Keys.spam, spamCounter)
        return node
    }
}

//MARK: JSON
extension User: JSONConvertible {
    convenience init(json: JSON) throws {
        try self.init(username: json.get(User.Keys.username),
                      email: json.get(User.Keys.email),
                      verifiedEmail: false,
                      passwordHash: json.get(User.Keys.passwordHash),
                      allergies: "",
                      spamCounter: 0)
    }
    
    func makeJSON() throws -> JSON {
        var json = JSON()
        try json.set(User.Keys.id, id)
        try json.set(User.Keys.username, username)
        try json.set(User.Keys.email, email)
        try json.set(User.Keys.verifiedEmail, verifiedEmail)
        try json.set(User.Keys.passwordHash, passwordHash)
        try json.set(User.Keys.allergies, allergies)
        try json.set(User.Keys.spam, spamCounter)
        return json
    }
}

//MARK: Response
extension User: ResponseRepresentable {}


