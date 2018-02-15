import XCTest
import Testing
import HTTP
import Cookies
@testable import Vapor
@testable import App

/// This file shows an example of testing 
/// routes through the Droplet.

class RouteTests: TestCase {
    let drop = try! Droplet.testable()

    func testInfo() throws {
        try drop
            .testResponse(to: .get, at: "info/request")
            .assertStatus(is: .ok)
            .assertBody(contains: "0.0.0.0")
    }
    
    func testLogin() throws {
        let request = Request(method: .post,
                              uri: "/login",
                              headers: ["Content-Type": "application/json"],
                              body: try Body(JSON(node: ["email": "jonas.peeters@me.com","password":"123456"])))
        try drop
            .testResponse(to: request)
            .assertStatus(is: .ok)
            .assertBody(contains: "Authenticated")
    }
    
    /// Used for authenticated routes
    func getSessionCookie() throws -> Cookie {
        let loginRequest = Request(method: .post,
                                   uri: "/login",
                                   headers: ["Content-Type": "application/json"],
                                   body: try Body(JSON(node: ["email": "jonas.peeters@me.com","password":"123456"])))
        
        return try drop
            .testResponse(to: loginRequest)
            .assertStatus(is: .ok)
            .assertBody(contains: "Authenticated")
            .cookies.array[0]
    }
    
    func testUserData() throws {
        let request = Request(method: .get, uri: "/user")
        request.cookies.insert(try getSessionCookie())
        try drop
            .testResponse(to: request)
            .assertStatus(is: .ok)
            .assertJSON("id", passes: { json in json.int != nil })
            .assertJSON("username", passes: { json in json.string != nil })
            .assertJSON("email", passes: { json in json.string != nil})
            .assertJSON("allergies", passes: { json in json.string != nil })
            .assertJSON("verfied", passes: { json in json.bool != nil })
            .assertJSON("spamCounter", passes: { json in json.int != nil })
            .assertJSON("password", passes: { json in json.string != nil })
    }
    
    
    
    override func setUp() {
        super.setUp()
        Testing.onFail = XCTFail
    }
}

// MARK: Manifest

extension RouteTests {
    /// This is a requirement for XCTest on Linux
    /// to function properly.
    /// See ./Tests/LinuxMain.swift for examples
    static let allTests = [
        ("testInfo", testInfo),
        ("testLogin", testLogin),
        ("testUserData", testUserData)
    ]
}
