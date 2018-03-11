import JSON
import Vapor

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
