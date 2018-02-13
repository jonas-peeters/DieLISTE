import JSON
import Vapor

/// Generates a JSON object from a string to allow for easier parsing on the clinet side
///
/// JSON encoding:
///
///     {
///       "error": $MESSAGE
///     }
///
/// - parameters:
///   - message: The error message
/// - returns: The error message wrapped in JSON
func generateJSONError(from message: String) -> JSON {
    var json = JSON()
    do {
        try json.set("error", message)
    } catch {
        print("Could not create error message")
    }
    return json
}

/// Tries to generate a JSON object from any given object
///
/// - parameters:
///   - content: The object that should be encoded
/// - returns: The object in JSON encoding if possible
/// - throws: Throws when the object can't be encoded as JSON
func makeJSON(from content: Any) throws -> JSON {
        var json = JSON()
        try json.set("content", content)
        return try json.get("content")
}


