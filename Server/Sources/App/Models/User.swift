import Vapor
import FluentProvider
import AuthProvider

/// # User
///
/// This is a model of all the properties a user has
public final class User: Model, SessionPersistable, PasswordAuthenticatable {
    /// Storage for internal vapor usage
    public let storage = Storage()
    
    /// The users name
    var username: String
    /// The user email
    var email: String
    /// If the email of the user is already verified
    var verifiedEmail: Bool
    /// The password of the user (hashed)
    var password: String
    /// The users allergies
    var allergies: String
    /// The number of times an ivite by the user was reported as spam
    var spamCounter: Int
    
    /// The lists the user has access to
    var lists: Siblings<User, List, Pivot<User, List>> {
        return siblings()
    }
    
    /// All items from all lists the user has access to
    var items: [Item] {
        var itemList: [Item] = []
        do {
            for list in try self.lists.all() {
                itemList.append(contentsOf: list.items)
            }
        } catch {
            return itemList
        }
        return itemList
    }
    
    /// All users this user has a list with in common
    var connectedUsers: [String : Int] {
        do {
            let lists = try self.lists.all()
            var users: [String : Int]  = [:]
            for userList in lists {
                for user in userList.users {
                    users[user, default:0]+=1
                }
            }
            return users
        } catch {
            return [:]
        }
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
    /// - throws: When the row can't be created possibly because of a issue when setting the keys and values
    /// - returns: A row containing all the information about the user
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
    /// - parameters:
    ///   - row: A row from the database
    /// - throws: When a key can't be read from the database
    /// - returns: The user
    public init(row: Row) throws {
        username = try row.get(User.Keys.username)
        email = try row.get(User.Keys.email)
        verifiedEmail = try row.get(User.Keys.verifiedEmail)
        password = try row.get(User.Keys.password)
        allergies = try row.get(User.Keys.allergies)
        spamCounter = try row.get(User.Keys.spam)
    }
}


// MARK: Fluent Preparation
extension User: Preparation {
    /// Prepares the database for using the user model
    /// - parameters:
    ///   - database: The database that should be prepared
    /// - throws: When the database can't be created
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
    /// - parameters:
    ///   - database: The database where the preparation should be reverted
    /// - throws: When the database can't be reverted
    public static func revert(_ database: Database) throws {
        try database.delete(self)
    }
}


// MARK: Node
extension User: NodeRepresentable {
    /// Makes the user model node representable
    ///
    /// - parameters:
    ///   - context: The context for the node encoding
    /// - throws: Throws if there is an error when setting the keys or values
    /// - returns: The user as a node
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
    /// Creates a user from a JSON object of the user
    ///
    /// Required json structure:
    ///
    ///     {
    ///       "email": $EMAIL_String,
    ///       "username": $USERNAME_String,
    ///       "password": $PASSWORD_String
    ///     }
    ///
    /// - parameters:
    ///   - json: A user encoded as JSON
    /// - throws: Throws if the json can't be parsed as a user
    /// - returns: The user
    public convenience init(json: JSON) throws {
        try self.init(username: json.get(User.Keys.username),
                      email: json.get(User.Keys.email),
                      verifiedEmail: false,
                      password: json.get(User.Keys.password),
                      allergies: "",
                      spamCounter: 0)
    }
    
    /// Creates a JSON object from a user
    ///
    /// - throws: If something goes wrong when setting keys and values
    /// - returns: The user encoded as JSON
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
