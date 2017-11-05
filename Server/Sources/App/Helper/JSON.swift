import Foundation
import JSON

func generateJSONError(from message: String) -> JSON {
    var json = JSON()
    do {
        try json.set("error", message)
    } catch {
        print("Could not create error message")
    }
    return json
}

func makeJSON(from content: Any) throws -> JSON {
        var json = JSON()
        try json.set("content", content)
        return try json.get("content")
}

