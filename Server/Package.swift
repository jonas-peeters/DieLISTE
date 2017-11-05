// swift-tools-version:4.0

import PackageDescription

/// # Package info
/// In this part of the code information about the package are stored.
///
/// This includes the name ("Server"), targets ("App", "Run" and "AppTests")
/// and also the following dependencies:
/// * Vapor
/// * MySQL
/// * Fluent-Provider
let package = Package(
    name: "Server",
    products: [
        .library(name: "App", targets: ["App"]),
        .executable(name: "Run", targets: ["Run"])
    ],
    dependencies: [
        .package(url: "https://github.com/vapor/vapor.git", .upToNextMajor(from: "2.1.0")),
        .package(url: "https://github.com/vapor/mysql-provider.git", from: Version("2.0.0")),
        .package(url: "https://github.com/vapor/fluent.git", from: Version("2.0.0")),
        .package(url: "https://github.com/vapor/fluent-provider.git", .upToNextMajor(from: "1.2.0")),
        .package(url: "https://github.com/IBM-Swift/Swift-SMTP.git", from: (Version("1.1.3"))),
    ],
    targets: [
        .target(name: "App", dependencies: ["Vapor", "FluentProvider", "MySQLProvider", "Fluent", "SwiftSMTP"],
                exclude: [
                    "Config",
                    "Public",
                    "Resources",
                ]),
        .target(name: "Run", dependencies: ["App"]),
        .testTarget(name: "AppTests", dependencies: ["App", "Testing"])
    ]
)

