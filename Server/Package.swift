// swift-tools-version:4.0

import PackageDescription

let package = Package(
    name: "Server",
    products: [
        .library(name: "App", targets: ["App"]),
        .executable(name: "Run", targets: ["Run"])
    ],
    dependencies: [
        .package(url: "https://github.com/vapor/vapor.git", from: Version("2.1.0")),
        .package(url: "https://github.com/vapor-community/postgresql-provider", from: Version("2.1.0")),
        .package(url: "https://github.com/vapor/fluent-provider.git", from: Version("1.2.0")),
        .package(url: "https://github.com/vapor/auth-provider.git", from: Version("1.2.0")),
        .package(url: "https://github.com/vapor/leaf-provider.git", from: Version("1.1.0")),
    ],
    targets: [
        .target(name: "App", dependencies: ["Vapor", "FluentProvider", "PostgreSQLProvider", "AuthProvider", "LeafProvider"],
                exclude: [
                    "Config",
                    "Public",
                    "Resources",
                ]),
        .target(name: "Run", dependencies: ["App"]),
        .testTarget(name: "AppTests", dependencies: ["App", "Testing"])
    ]
)

