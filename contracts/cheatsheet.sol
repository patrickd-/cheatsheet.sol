// SPDX-License-Identifier: GPL-3.0
// ^ Recommended. Included machine-readably in bytecode metadata (at end of bytecode)
//   May be omitted with solc option "--no-cbor-metadata"
// Software Package Data Exchange (SPDX) is an open standard

// =================================== HOW TO USE ===================================
// ||  Open Remix (https://remix.ethereum.org/) and select the default_workspace.  ||
// ||  Copy cheatsheet.sol contents and paste them into a new file in /contracts   ||
// ||  Ignore the "pasting is dangerous" message (no worries, we're the good guys) ||
// ||  After saving, switch to "Solidity compiler" in the sidebar, click "Compile" ||
// ||  Switch to "Deploy & run transactions" and have fun reading and exploring!   ||
// ==================================================================================

pragma solidity ^0.8.27;
// ^ "floating pragma", (means: min 0.8.27, max excluding 0.9.0)
// Same as complex pragma: pragma solidity >=0.8.27 <0.9.0;
// Pattern: major.breakingchanges.bugfixes
// Only makes the compiler check for compatibility and throws error if not matching!
// Floating pragmas should only be used for libraries, and maybe early development.
// Consider using a current, but well tested, version (latest may have undiscovered bugs)

pragma abicoder v2;
// ^ v2 able to en/decode arbitrary nested arrays and structs, v2 is default since 0.8.0
// Contracts using v2 are backwards compatible with v1, as long as v1 contract doesn't
// try to call a v2 contract function that needs the new features (compiler errors)
// v1 contracts are allowed to inherit from v2 contracts as long as its only used internally
// Before 0.8.4 v2 ABI-decoding had a bug when decoding multi-dimensional arrays from memory.

// These files exist by default within /contracts of Remix's default_workspace.
import "./1_Storage.sol";
// ^ Imports Storage.sol's globals into the cheatsheet's global scope.
import * as ownerNamespace from "./2_Owner.sol";
// OR import "./2_Owner.sol" as ownerNamespace;
// ^ Imports Owner.sol's globals into a namespace without polluting this global scope.
import { Ballot } from "./3_Ballot.sol";
// OR import { Ballot as BallotContract } from "./3_Ballot.sol";
// ^ Import *specific* global symbols into this global scope, optionally renaming them.

// Remix is able to automatically import from NPN and URLs (like GitHub, even IPFS, or Swarm).
// import "@openzeppelin/contracts@4.2.0/token/ERC20/ERC20.sol";
//                                ^^^^^^ Specifying a version is optional
// import "https://github.com/OpenZeppelin/openzeppelin-contracts/blob/v2.5.0/contracts/math/SafeMath.sol";
// This is convenient for playing around but not part of standard Solidity!

// Interfaces are typically prefixed with an "I".
// May only inherit from other interfaces.
// They can't have state variables or constructors.
// Best practice dictates that NatSpec should be, at minimum, used on all public interfaces.
interface ITest {
    // Even if they're actually public in its implementation, an Interface's functions
    // must be annotated as external. Functions in interfaces are implicitly virtual.
    function payMe() external payable;
}

// Abstract Contracts may be missing the body of one or more functions (to be implemented by a child).
// "abstract" and enforced usage of "virtual"/"override" keywords was introduced in 0.6.0
// Like interfaces, abstract contracts cannot be deployed but serve to organize a codebase.
abstract contract TestBase {

    // Virtual functions of abstract classes without body MUST be implemented by non-abstract Contracts.
    function nobody() external view virtual; // Must be explicitly virtual if without body.

    // Virtual functions that already have a body MAY be overriden.
    function _overrideMePlease(uint a) internal pure virtual returns (uint) {
        return a;
    }

    // Can be overridden by a public state variable if its of the same return-type.
    function overridableByStateVariable() external virtual returns (uint) {
        return 1;
    }

    // Virtual modifiers can be overridden but not overloaded.
    modifier checker() virtual;

}

contract TestBase2 {
    modifier checker() virtual {
        _;
    }
}

// Non-Abstract Contracts must have all function bodies implemented.
contract AdvTestBase is TestBase, TestBase2 {

    // Overrides an external function of the same name returning the same type.
    uint public override overridableByStateVariable;

    // Implementing ("overriding") a bodyless function from an abstract Contract is required.
    // If we don't mark it as "virtual" again the inheriting Contract may not override it.
    // When overriding functions: External may become public, mutability may become stricter (if not payable).
    function nobody() public pure override {
        // nop.
    }

    // Function is overriden but kept virtual to allow for further overrides.
    function _overrideMePlease(uint a) internal pure override virtual returns (uint) {
        // A Base Contract implementation, one level higher up in the flattened inheritance hierarchy,
        // can be called using super. Alternatively a specific Base Contract can be specified.
        return super._overrideMePlease(a) ** 2;
        // ^ Equivalent to calling: TestBase._overrideMePlease()
    }

    // When overriding a function or modifier defined in multiple parallel bases, all bases must be listed:
    modifier checker() override(TestBase, TestBase2) {
        _;
    }
}

/**
 * @title Test, a Contract is similar to a Class in Object Oriented languages
 * @author patrickd (Ventral Digital LLC)
 * @notice NatSpec example – a notice for endusers, becomes part of JSON ABI to be used in UIs
 * @dev "NatSpec" stands for Ethereum Natural Language Specification Format
 * @custom:example Custom annotations start with "custom", end with anything your app wants
 *
 * This is this file's main Contract. The general best practice is to give files the name of their
 * main contract: Test.sol – For better readability, a file should only contain a single contract.
 *
 * You may not declare state variables with a name used in a base contract (shadowing)!
 *
 * Inheritance means that components of the parent contracts are "merged" into the inheriting contract!
 * The parent contracts do not need to be deployed, since everything will be contained in the child.
 * Order of "merging" is that the right most contracts override those on the left (C3 Linearization).
 *
 * The order of inheritance should start with the least derived contract (most general, usually an
 * interface) and end with the most specific, to avoid inheriting the incorrect function implementation.
 */
contract Test is ITest, AdvTestBase, Storage, ownerNamespace.Owner, Ballot {

    // It's a common best practice that within contracts, libraries and interfaces, there should an order:
    // Start with Type declarations, then State variables, Events, and finally Functions.
    // Functions should be ordered such that the constructor() comes first, then receive() and fallback(),
    // then based on function visibility: external, public, internal, private. Within each of these
    // groups of functions, view and pure functions should come last.

    // Solidity is "statically"-typed, that means all types must be known at compile time.
    // There's no concept of "null" or "undefined".
    // All "empty" variables are well defined in "zero-state" – it may still be preferable to explicitly
    // assign them a value (even zero) to avoid incorrect assumptions or unintended use of zero-addresses.
    // An exception to this rule are upgradable contracts where the constructor wouldn't be executed in
    // the proxy's context and initialize functions are used instead – assign values in initialize()!
    uint public pubInt = 0;         // Automatically has a public getter func of same name.
                                    // Only public state vars can have NatSpec comments (since 0.7.0)
    bool internal _intBool = false; // Can only be used from this or inheriting contracts.

    uint internal _mutex = 1;   // See reentrancyGuard modifier.

    bytes32[3] private _privA; // Contains 3, each 32 bytes long "bytearrays" filled with zeros,
                               //   because it's statically sized!
                               // Can only be used within this Test contract (because "private"),
                               //   don't forget that all data on the blockchain is actually public!
                               // Arrays (also bytes and strings) are "reference" types!

    uint8 public constant CONST = 42; // Is placed everywhere it's used (in the bytecode) during compile time.
    uint8 internal immutable _immu;   // Becomes immutable after being set during construction.
                                      // Is placed into 32 byte placeholder slots when set,
                                      //   meaning in this 8bit case the constant is cheaper!
                                      // Both are placed directly into the runtime bytecode,
                                      //   that means no storage is used!

    // Only value types are supported by immutables, but constants support strings too!
    string private constant _FOOBAR = "foobar!";

    // Depending on their order, state variables can be packed into the same slot (starting at slot 0).
    // Packing reduces storage slot usage but increases opcodes necessary to read/write to them.
    uint248 _right; // 31 bytes. If it doesn't fit into the previous storage variable slot: It get's a new one.
    uint8   _left;  // 1 byte.   Each storage slot is 256 bit, this variable only needs 8: It get's packed!
    // ^ A single storage slot will be packed from right to left with these two values (lower-order aligned).
    // Structs and array data always start a new slot!
    // Dynamically-sized arrays use their slot at index p for storing their length,
    //   their values start being stored at index keccak(p).
    // Mappings leave their slot at p unused (to avoid clashes), their individual values are stored at
    //   keccak(h(k) + p) with function h() either padding value types or hashing reference types.
    // Bytes and Strings larger than 31 bytes work like arrays, but when smaller, the bytes of smaller ones
    //   are stored in the higher order bits of their slot. The length value at the lower order bits.
    // Same packing rules apply for inherited state variables, they are as if "merged" in their C3-linearized
    //   order, starting with the most base-ward contract.
    // Note there's no packing in memory! Memory is always padded to 32 bytes, and there's no Garbage Collection!

    /**
     * A constructor is executed once with a transaction, generating the runtime bytecode which is then actually
     * deployed. All internal functions only called by the constructor are not included in the runtime bytecode.
     *
     * The constructors of all the base contracts will be called following the linearization rules,
     * starting with the most base-ward Contract: ITest, AdvTestBase, ..., Ballot, Test
     *
     * If the base constructors have arguments, derived contracts need to specify all of them either
     * in the inheritance list or here in the derived constructor. If the base contracts don't have
     * a constructor implemented you shouldn't call them to prevent misplaced assumptions.
     *
     * Until solc <0.5.0, the constructor could also be the name of the class instead of using the
     * constructor keyword. If that "named constructor" was not exactly matching the class name
     * it would end up as a public function in the runtime bytecode, a potential vulnerability!
     *
     * Constructors may call other functions of the contract they're part of. Since 0.8.19 functions
     * that are ONLY called during construction will not be part of the runtime code anymore.
     */
    constructor(bytes32[] memory proposalNames, uint8 immuV) Ballot(proposalNames) {
        _immu = immuV + 2; // Before 0.8.8, immutable variables could only be set, but not read from
                            //   during construction. Now they can be used like any other variable.
                            // Since immutable variables are placed into the runtime bytecode,
                            //   they will become read-only in the deployed contract.

        // Inline assembly is a way to access the Ethereum Virtual Machine at a low level.
        // The language used for inline assembly is called Yul which has improved readability.
        // Use of EVM assembly is error-prone and should be avoided or double-checked for correctness.
        //
        // Since 0.8.13 assembly blocks can be marked as "memory-safe" this will allow the compiler
        // for better gas optimizations. Yul-code MUST follow Solidity's memory model in this case!
        //
        // /// @solidity memory-safe-assembly
        // ^ This annotation added directly above an assembly block allows marking the block as
        // memory-safe while maintaining backwards compatibility with previous solidity versions
        // which is useful for libraries that have a pragma supporting versions <0.8.13
        //
        // The future-proof way to mark an assembly block is: `assembly ("memory-safe") {`
        // because support for the annotation will be removed in some future version.
        assembly ("memory-safe") {
            // Yul parses comments, literals (since 0.8.4) and identifiers in the same way as Solidity.
            // You can access Solidity variables and other identifiers by using their name.
            // Local variables of value type (ie. not references) are accessible in inline assembly:
            let numba := immuV // < No ; semicolons!
            //^       ^ assignments with ":=" instead of "="!
            //^ Uses the "u256" type, which is the native 256-bit type of the EVM.

            // Local variables that refer to memory/calldata evaluate to the address of the
            // variable in memory/calldata and not the value itself!
            let numberOfProposals := mload(proposalNames) // Elements in memory arrays in Solidity
                                                          //   always occupy multiples of 32 bytes.
                                                          // The length of a dynamic array is stored
                                                          //   at the first slot of the array and
            let proposal1 := mload(add(proposalNames, 32))//   followed by the array elements

            // For local storage variables or state variables, a single Yul identifier is not sufficient,
            // since they do not necessarily occupy a single full storage slot. Therefore, their “address”
            // is composed of a slot index and a byte-offset inside that slot.
            let rightAndLeftValues := sload(_left.slot)
            // To get the value on the rightmost side of the slot, we can use a mask to remove the left value.
            // Although Solidity will take care of masking the first byte when reading a uint248, we should
            // remove it to ensure there are no "dirty" upper bits that can cause issues somewhere else.
            let rightValue := and(rightAndLeftValues, 0x00ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff)
            // To get the value on the leftmost side of the slot, we have to shift it to the right.
            // While shifting, the rightmost value will "fall out" while the left side is padded with zeros.
            // _left.offset will be 31, therefore 31*8 is 248 bits that have to be shifted:
            let leftValue := shr(mul(_left.offset, 8), rightAndLeftValues)
            // Shift operators (shl(x, y), shr(x, y), sar(x, y)) in Solidity assembly apply the
            // shift operation of x bits on y and not the other way around, which may be confusing.
            // Make sure you have the parameters in the right order!

            // You can assign to the .slot part of a local storage variable pointer.
            // For these (structs, arrays or mappings), the .offset part is always zero. It is not
            // possible to assign to the .slot or .offset part of a state variable, though:
            // pubInt.slot := 0x0 // Not allowed, use sstore() instead.

            // Reserved Memory: Solidity reserves four 32-byte slots, with specific byte ranges
            // (inclusive of endpoints) being used as follows:
            // 0x00 - 0x3f (64 bytes): Scratch space for hashing methods
            // 0x40 - 0x5f (32 bytes): Currently allocated memory size (aka. free memory pointer)
            // 0x60 - 0x7f (32 bytes): Zero-Slot (Copied from as initial value for dynamic memory arrays
            //                                    and should never be written to)
            //
            // There is no guarantee that "free" memory has not been used before and thus you cannot
            // assume that its contents are zero bytes. There is no built-in mechanism to release
            // or free allocated memory.
            //
            // The free memory pointer will start pointing to the address immediately after the
            // reserved memory slots, to 0x80. Although any addresses can be accessed (not only
            // chunks of 32 bytes), Solidity always uses memory in 32-byte-chunks and you should
            // do so too: Don't forget to increase the pointer by 32 for each new chunk written to!
            // Otherwise Solidity will overwrite your memory slots thinking its allocating free space.
            let freeMemoryPointer := mload(0x40)
            mstore(freeMemoryPointer, 42) // Storing the answer to life the universe and everything.
            mstore(0x40, add(freeMemoryPointer, 32)) // Reserve memory space now used by the answer.

            // Although there are if-statements in Yul, they don't have "else":
            if eq(freeMemoryPointer, 0x80) { /*...*/ }
            // Instead, switch statements offer "default":
            switch freeMemoryPointer
            case 0x80 { /*...*/ }
            default { /*...*/ }

            // Check the official Yul documentation for more about inline assembly loops and functions
            // https://docs.soliditylang.org/en/latest/yul.html

            // 0.8.3 had a bug where keccak256 hashes made within inline assembly would be cashed for
            // the same memory location even if the length to be hashed differed which should have resulted
            // in a different hash.

            // 0.8.13 and 0.8.14 had a bug where memory writing operations in YUL were removed if they
            // were not read from within the same inline assembly block.
            mstore(0x420, 1) // 0x420 is not accessed again here, this operation would've been removed.
            // Happened when no surrounding Solidity variable is accessed and --via-ir was not used.
        }
    }

    /**
     * @notice returns what you pass
     * @dev echo!
     * @param test - any positive num. function parameters are like local vars and can be reassigned
     * @return will return test. is unnamed here and needs to be returned with return keyword
     *
     * Public means it can be called externally AND internally!
     * Since they can be called by ANYONE, make sure that you have appropriate access control!
     *
     * Externally callable functions are added to the "function selector" which is basically a
     * switch-case at the beginning of a contract's bytecode routing an external call to the
     * appropriate function by jumping to it. "Function signatures" are used to identify the
     * function to jump to: signature = bytes4(keccak("echo(uint)"));
     *
     * An internal call is equivalent from jumping from one function to another without CALLing.
     *
     * Prior to <0.5.0 specifying a visibility was optional and the default was public causing
     * vulnerabilities from incorrect visibility assumptions.
     */
    function echo(uint test) public pure returns (uint) {
        // Shadowing of state variables within functions only causes a warning but should be avoided.
        // uint pubInt;
        // A common practice is to pre or append underscores to such variables:
        uint _pubInt = test;

        /// @notice single line NatSpec comment with 3 slashes, while blocks have two asterisks (/**).
        return _pubInt;
    }

    /**
     * Return variables can be pre-named and set like normal local variables, which will cause
     *   them to be initialized with the default zero-like state. No explicit return statement
     *   is necessary in this case. This pattern is often considered a bad practice though!
     *
     * Best-practice: Don't declare named return variables if you don't actually use them.
     *                And aim for consistency, either use them everywhere or nowhere.
     *
     * We're also using Function Overloading here! Same name, different parameters!
     *
     *
     * External means it can only be called via messages! (ie. transactions or other contracts).
     *   We can force calling it from the same contract using `this.echo(uint8(1))`
     *   which will cause the contract to make an (expensive) external call to itself.
     *
     * Since uint8 is a value type, no data location can be specified!
     *   Externally callable function (even public ones, though that rarely makes sense) may make
     *   use of the "calldata" data location instead of "memory".
     *   This saves gas since it skips the automatic copying to memory when called.
     */
    function echo(uint8 test) external pure returns (uint8 testr) {
        // If we just set the return value, but never the return statement, the value is still returned.
        testr = test;
        // There's no implicit type conversion to booleans! ie. `if (test)` won't work.
        // So for conditionals we have to make explicit comparisons that result in booleans.
        if (test == 1) {
            return testr; // If we WANT to return(), we MUST specify the return value anyway;
        }
    }

    /// @dev example with multiple return values
    // A whitespace separated list of modifiers can be applied to functions.
    // Modifiers that don't have parameters are called without "()".
    function echo(uint16 test) public freeChecker paramCheck(test) returns (uint8 test8, uint16 test16, uint32) {
        test16 = test;
        test8 = uint8(test); // explicit conversion
        return ( // count, order and types must match!
            test8,
            test16,
            test // implicit conversion
        );
    }

    /**
     * If a modifier is used for many functions and it contains complex logic,
     * it's most likely best to extract its code into a separate function to
     * reduce bytecode, since it'll be pre- and appended to all funcs using it.
     *
     * Consider using well established libraries that can provide you with modifiers
     * for various usecases, eg.: OpenZeppelin's Ownable, AccessControl, Pausable,..
     *
     * Modifiers can be overridden but not overloaded!
     *
     * Modifiers should not have side effects, they should only implement checks.
     * Side-effects (calls, state changes) may go unnoticed by developers/auditors
     * because the modifier code is typically far from the function implementation.
     * The regular exception to this best practice are reentrancy guards (mutex).
     */
    modifier freeChecker {
        // A function annotated with this modifier will be prefixed with the code of that
        // modifier up to the point where the underscore (_) appears.
        // A modifier may choose to revert before reaching _;
        _doStuff();
        // Continue to execute the function body using this modifier.
        _;
        // We can even execute the function's body multiple times!
        _;
        // Or we can skip its execution completely without reverting:
        if (false) _; // When skipped the return values will stay in previous/default state.
                      // Modifiers that don't execute the function body and return default
                      // values, may cause unexpected behavior!
        // It's not possible to completely omit the underscore though, that's a syntax error.
    }
    function _doStuff() private {
        _freeFunction(_privA);
    }

    // This modifier takes parameters (see the echo function on how values are passed).
    modifier paramCheck(uint test) {
        require(test > 0);
        _;
    }

    /**
     * State variables such as a reentrancy guard mutex change often within the same transaction.
     *
     * For such cases it is best to set a non-zero value during construction since changing a state
     * variable from zero to a non-zero value is much more expensive than toggling between two
     * different non-zero values.
     *
     * Since only a certain amount of gas is reimbursed for freeing storage (refund quota), it's
     * also better to not waste that quota here for toggling.
     */
    modifier reentrancyGuard() {
        require(_mutex == 1);
        _mutex = 2;
        _;
        _mutex = 1;
    }

    /**
     * Ethereum's Cancun upgrade introduced "Transient Storage", which basically behaves just like
     * normal contract storage except that its contents are deleted at the end of the transaction
     * and that it's cheaper to use than normal, permanent storage.
     *
     * As of 0.8.24 functions have been added to Yul allowing to access this storage type, there's
     * as of 0.8.27 not yet any way to use it with pure Solidity. The compiler will also always
     * emit a warning message when these are used at the moment due to the danger of incorrect use.
     */
    modifier transientReentrancyGuard() {
        assembly("memory-safe") {
            // Mutex is locked? Revert!
            if tload(0xf00b44) {
                mstore(0x00, 0xab143c06) // `Reentrancy()` error signature
                revert(0x1c, 0x04)
            }
            // Mutex is free? Lock!
            tstore(0xf00b44, 1)
        }
        _;
        assembly("memory-safe") {
            tstore(0xf00b44, 0)
        }
    }

    /**
     * View functions are executed using the STATICCALL opcode which ensures they
     * can only read blockchain state and not write to it.
     *
     * Prior to 0.5.0 no STATICCALL was used and state changes could be made
     * stealthily within inline assembly.
     */
    function noTouching() public view returns (uint) {
        // pubInt++;         // Nope!
        return block.number; // Okay!
    }

    /**
     * Pure functions may only access CALLDATA
     *
     * Pure doesn't even allow read access, but this is only enforced by the
     * compiler! Meaning a function may claim to be pure but may actually
     * read state like a view function.
     */
    function borderlinePure() public pure returns (bytes calldata, bytes4) {
        return (
            // Using information from the transaction is okay.
            msg.data,        // Okay!
            msg.sig        // Okay!
            // Accessing information from chain state is not.
            // msg.sender,  // Nope!
            // msg.value,   // Nope!
            // tx.gasprice, // Nope!
        );
    }

    /**
     * Events are "logged", written to the blockchain and should be emitted for all
     * critical state changes with appropriately indexed topics.
     *
     * Can only be created via Contracts and can only be read from off-chain through
     * a node's RPC interface, by subscribing to them.
     *
     * Up to 3 "topics" may be indexed + the account address, allowing applications to
     * filter for specific events when subscribing. (Only the hash of topics is stored!)
     * Non-Indexed topics go into the raw data part of that log, but can't be filtered.
     */
    event ChingChing(address indexed sender, uint moneyz);

    /**
     * If you have a function that will be called very often, in order to safe gas..
     *  - Make use of "calldata" location for external function parameters.
     *    This will avoid expensive memory copying operations when the function is called.
     *  - Declare external functions ordered by how likely they are to be called, most to least.
     *    The order influences how fast the function selector will be able to find the called function.
     *  - Declare functions as payable in order to omit the value-is-zero check that is added by default.
     *    Best practice: Make sure there's a way to withdraw unintentionally sent ether though.
     *  - Name the function so its signature has many zeros as a prefix (deposit_CIx() = 0x00007693)
     *    Tool: https://emn178.github.io/solidity-optimize-name/
     *    Make sure you have no function signature clashes! (ie. functions with same signature)
     *  - Have it make use of events instead of storage as much as possible.
     */
    function deposit_CIx() external payable { // solhint-disable-line func-name-mixedcase
        emit ChingChing(msg.sender, msg.value);
    }

    /**
     * When "delete" is applied on a struct, all its elements are reset to zero-state,
     *   except if they are a mapping!
     *
     * Structs and array data always have their own slot and their items are packed tightly
     *
     * A struct is a reference type!
     */
    struct MagicGroupOfVariables {
        uint unsignedFun;
        address whereDoesItPoint;
        Hat magicHat; // A struct can contain other structs but not itself!
        Colors poof;
        // mapping(uint=>uint) map; // Structs can contain mappings but then
                                    // the struct must be in storage!
                                    // Before 0.7.0 mappings were simply skipped.
    }
    struct Hat {
        bool containsRabbit;
    }

    // Basically an uint8 with names for each number.
    // Can have 1 to 256 elements (upper limit introduced in 0.8.0)
    // Default / zero state is 0, ie. the first element.
    // Is a value type despite looking like a struct!
    // Since 0.8.8 .min and .max access the first and last enum values.
    enum Colors {
        Red,     // Colors.min
        Green,
        Blue     // Colors.max
    }

    function _doMagic() private pure {
        // Since this is a reference type, we need to specify a writeable data location! (memory or storage)
        MagicGroupOfVariables memory g1;    // Initialized with zero-like defaults by declaration.
        g1.whereDoesItPoint = address(0x0); // Zero values can more freely be cast into addresses and bytearrays
        // ^ Best Practice: Prevent unintended burning by disallowing zero addresses as destinations!
        g1.magicHat.containsRabbit = true;  // Access sub-members with dot notation.
        g1.poof = Colors.Blue;

        // Struct initialization possible with specific values.
        Hat memory h1 = Hat(true);
        Hat memory h2 = Hat({ containsRabbit: true });
        delete h1; // Resets to default (zero) values, except when struct contains mappings.
        delete h2;
    }

    /**
     * Receive function, triggered when receiving ether to no specific function.
     *
     * Must be declared exactly like this, no function keyword, no params, no return!
     *
     * Executed on empty CALLDATA (usually by send() / transfer() calls)
     * Only receives 2300 gas from those, barely enough for some logging.
     *
     * No receive function? Contract can still receive ether!
     * - any other payable functions, obviously.
     * - from selfdestruct() – which does not call the receiver when sending ether to it, it can't reject
     * - before the merge: as block reward beneficiary (coinbase)
     * - after the merge: as receiver of Proof of Stake rewards
     * - if the receiving contract currently under construction
     *
     * Best practice: If you have payable functions make sure that there's also a way to withdraw
     * the ether, otherwise it might be stuck forever!
     *
     * Exists since 0.6.0, before there was the unnamed fallback function:
     *   function() public payable {}
     */
    receive() external payable {
        emit ChingChing(msg.sender, msg.value);
    }

    // Similar to receive.
    // Payable, calldata parameter, and return value are optional though.
    // Executed when no function matches the signature specified in calldata.
    // Also executed on empty CALLDATA but only if there's no receive().
    // Could also only receive 2300 gas, if there's no receive().
    fallback(bytes calldata _input) external payable returns (bytes memory _output) {
        if (msg.value > 0) emit ChingChing(msg.sender, msg.value);

        // Array slicing (index range) is only available for dynamic calldata arrays: (since 0.6.0)
        bytes memory slicinAround = _input[:]; // Full copy with [0:_input.length] defaults
        slicinAround = _input[0:4]; // Function 4-byte-signature from calldata (same as msg.sig)
        slicinAround = _input[4:];  // Function argument data (all bytes after 4-byte-signature)
        // Array slices are unnamed types with no members and scoped references to the underlying
        //  array (relative index access).

        return slicinAround;
    }

    // The payMe() function is specified in the ITest interface that this contract implements.
    // Until 0.8.8 it was required to mark the implementation of such a function with "override".
    function payMe() external payable /* override */ {
        // Note that msg.sender and msg.data might not have the correct information
        // when accessed in library-like contracts if meta-transaction are used.
        // Consider accessing these values via OpenZeppelin Context library instead.
        emit ChingChing(msg.sender, msg.value);
        // Also note the danger of accessing msg.value within a loop. If, for example,
        // this contract implements a function allowing to batch multiple calls into one
        // it would allow calling payMe() multiple times with the same msg.value making
        // it look like the sender paid multiple times although he only send value once.
    }

    function checkBalance() external view returns (uint) {
        return address(this).balance;
    }

    /**
     * Mappings are reference types that can only be in storage.
     *
     * Keys are hashed and must be value types!
     * Not iterable without an additional Array tracking its keys.
     * That also means no length.
     *
     * Known items are deletable via keys but attempting to delete the
     *   entire mapping has no effect.
     * Values can be of any type and the value of any unused key is zero-like.
     *
     * If you need more control over the mapping consider using OpenZeppelin's
     * EnumerableMap or EnumerableSet libraries!
     */
    mapping(address => MagicGroupOfVariables) _magicians;

    /**
     * Since 0.8.18 the parameters of mappings may be named!
     *
     * This has no further effect than improving readability.
     */
    mapping(address magicianHome => MagicGroupOfVariables magicianState) _magiciansWithNames;

    function boolingAround(bool a, bool b, bool c) public pure returns (bool) {
        // Short-circuit rule: it won't evaluate/check the rest of the variables
        //   when the result is already determined by previous variables
        return a && (b || c);
    }

    // Integers come in sizes between 8 to 256 bit, in steps of 8.
    function iHaveAnIntling() public pure returns (uint, uint, int, int) {
        return(
            // uintX range
            type(uint16).min, // 0
            type(uint16).max, // 2**X - 1
            // intX range
            type(int16).min, // (2**X)/2 * -1
            type(int16).max  // (2**X)/2 - 1
        );
    }

    // Aside from obvious math arithmetic operators, integers also support:
    //   Bitwise & (AND)
    //   | (OR)
    //   ^ (XOR)
    //   ~ (NEG)
    //   Shift << (left) >> (right)
    //
    // We can make use of these to manage bitmaps!
    // Note there's well tested libraries like OpenZeppelin's BitMaps for this!
    //
    // Bitmap Getter function: Check whether a bit in the map is set.
    function bitmapGet(uint256 map, uint8 index) external pure returns (bool) {
        // Example using function input of a map of ..11000101 and index 2:
        // 1) Create Mask: Left-shift 2 the number one (..00000001 becomes ..00000100)
        // 2) Apply Mask on map: ..11000101 AND 00000100 = 00000100
        // 3) Value greater than zero? -> true
        return map & (1 << index) > 0;
    }
    // Bitmap Setter function: Set or unset a bit in the map.
    function bitmapSet(uint256 map, uint8 index, bool val) external pure returns (uint256) {
        if (val) {
            // We want to set the bit to 1. Same principle as getter, but this time we use OR
            // which will always return true for the bit at that position, even if its already set.
            return map | (1 << index);
        }
        else {
            // To unset, we once again use AND, but with an inverted bit mask:
            // ..00000100 becomes ..11111011
            return map & ~(1 << index);
        }
    }

    // User Defined Value Types were introduced with 0.8.8
    type Price is uint128; // Behaves exactly as a normal uint128 when ABI encoded or as part of signature.
    // ^ 0.8.9 fixed an issue where underlying types smaller than 32 bytes used incorrect storage layout, wasting gas.

    // Apply library to a type, but only within this contract (was inherited before 0.7.0).
    using UnsafeMath for Price; // Works for all types! (See end of file for UnsafeMath library)
    // ^ Since 0.8.13
    //    - Not only libraries but also free functions (ie. functions outside of contracts) and specific
    //      library function may be applied to a type:
    //      using { _freeFunction, UnsafeMath.add } for Price;
    //    - Can be declared outside of a contract's scope, applying to the entire file.
    //    - Can additionally be declared as "global", applying to files that import it as well.
    //    - If the type is User Defined, that type's declaration must be in the same file as its global usage.

    // Before 0.8.19, User Defined Value types did not support operators (ie. you were not able to compare
    //   (using ==) or add (using +). This is now possible:
    // using { UnsafeMath.add as + } for Price global; // Must use "global" and be declared outside of contract scope.

    /**
     * The User Defined Value Type "Price" does not exist in the ABI specification or
     * the function signature of this function!
     *
     * Instead they will make use of the underlying type, which is uint128.
     * True Function Signature is based on: "testUnsafeMath(uint128,uint128)"
     */
    function testUnsafeMath(Price a, Price b) public pure returns (Price subbed, Price diffedUp, Price diffedRight) {
        // Results from libraries are returned and not applied to the value!
        subbed = a.sub(b); // Calls sub(a, b) on the UnsafeMath library

        // If you are looking for more math functions that are typically provided
        // look for libraries such as OpenZeppelin's Math (max(), min(), avg()).

        // When working with User Defined Value Types, we have to unwrap/wrap the underlying value!
        // Performing multiplication before division is generally better to avoid
        // loss of precision because Solidity integer division might truncate.
        diffedUp = Price.wrap((Price.unwrap(a) / 255) * Price.unwrap(b));                          // (7 / 255) * 255 = 0
        diffedRight = Price.wrap(uint128((uint(Price.unwrap(a)) * uint(Price.unwrap(b))) / 255));  // (7 * 255) / 255 = 7
    }

    /**
     * Private functions are often prefixed with underscore.
     * Since they are only available within their contract, they cannot be virtual.
     *
     * Due to a compiler bug, prior to <v0.5.17 private functions could override functions
     * in base contracts although they shouldn't be accessible at all.
     */
    function _fixMe() private pure returns (ufixed nope, fixed doubleNope) {
        // Fixed Point Number types exist but cannot be used, aside from being declared.
        // Use libraries for them instead! eg. DSMath, PRBMath, ABDKMath64x64
    }

    // Prefer Fixed-size "byte arrays" when possible!
    function _byteMe() private pure returns (bytes32, bytes1[32] memory) {
        bytes32 cheapByteArray;               // bytes1, 2, 3, ... 32 stored as single stack element, Value Type!
        bytes1[32] memory expensiveByteArray; // A full 32 byte memory slot is wasted for each of the 32 bytes
        // ^ "byte" alias of "bytes1" was removed in 0.8.0
        return(cheapByteArray, expensiveByteArray);
    }

    // The following shows how arrays in memory and in storage are quite different.
    // The type of an array can be anything, even structs and maps. Arrays are always reference types!
    uint128[3] private _sFixedArray;   // Specified size is fixed at compile time.
    uint128[] private _sDynamicArray;
    function arraysOfFun(uint size) public returns (uint128[3] memory, uint128[] memory) {
        // Dynamic storage arrays are resizable!
        _sDynamicArray.push(); // Empty push, appends a zero-state value!
                               // Constant gas cost thanks to zero-initialization
                               // Pre 0.6.0 push() returned the new length
        _sDynamicArray.pop();  // Implicit "delete" on last element
                               // Resets element to zero-state, can be very costly!

        // Pre 0.6.0 dynamic storage arrays could be resized by setting length member.
        // _sDynamicArray.length = 100;

        _sDynamicArray.push(1); _sDynamicArray.push(2); _sDynamicArray.push(3);
        // Example: Remove arbitrary array element (eg. first instead of last one):
        //          Replace it with last item, which is popped off (order is not maintained)
        _sDynamicArray[0] = _sDynamicArray[_sDynamicArray.length - 1];
        _sDynamicArray.pop();
        // Array has gone from 1, 2, 3 to 3, 2

        // Prior <v0.7.3 had a compiler bug when assigning a dynamically sized array with types of size
        // at most 16 bytes in storage causing the assigned array to shrink, some parts of deleted slots
        // were not zeroed out.
        _sDynamicArray = [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1];
        _sDynamicArray = [2];

        // Memory Array with fixed size at compile time.
        uint128[3] memory mFixedArray;
        mFixedArray[0] = 98; // Immediately ready to use!

        // Dynamic Memory Array is just a pointer to nowhere!
        uint128[] memory mDynamicArray;
        // mDynamicArray[0] = 99; // This will revert!

        // First we have to initialize it with an array with a specific size:
        mDynamicArray = new uint128[](size); // Once created, this array is not resizable!
        mDynamicArray[0] = 99; // Now this works!

        // So "dynamic memory arrays" are not dynamic in the sense that they can be resized.
        // They are dynamic in the sense that their size can be set during runtime, and in
        // the sense that you can have them point to a complete different array:
        mDynamicArray = new uint128[](1);
        mDynamicArray[0] = 42;

        // Parameters and return values should always have predictable sizes to avoid DoS.
        // Also never have loops without bounds for the same reasons!
        return (mFixedArray, mDynamicArray);
    }

    // Special arrays: "bytes" and "string", are also reference types!
    bytes _myBytes;   // Like bytes1[] but tightly packed (no wasted memory!).
                      // Use it for arbitrary length raw byte data.
    string _myString; // Like bytes but no string.length, no string[x] index access, no string.push().
                      // Use libraries for string manipulation!
                      // Use it for arbitrary length UTF-8 strings.
    // Solidity doesn't have any string manipulation members, consider
    // looking for a library for those, eg. OpenZeppelin's Strings.
    function bytesNstrings() public {
        // Creating bytes and string in memory:
        bytes memory myMemBytes = hex"f00b43";
        string memory myMemString = "foobar";
        // We can have solidity copy them to storage:
        _myBytes = myMemBytes;
        _myString = myMemString;

        // Bytes are like arrays, in that only storage bytes can be resized.
        _myBytes.push(0x0);
        // _myString.push(0x0);   // Nope for strings!
        // myMemBytes.push(0x0);  // Nope for memory!
        // myMemString.push(0x0); // Nope for memory!

        // Prior <v0.7.4 had a compiler bug when copying an empty byte array (or string) from
        // memory or calldata to storage could result in data corruption if the target array's
        // length was increased subsequently without storing new data:
        bytes memory emptyBytes;
        _myBytes = emptyBytes; // Copies dirty bits from memory to storage.
        _myBytes.push();       // Only increased length without setting storage (dirty bits) to 0.
        // _myBytes now had whatever came after emptyBytes in memory "added" to it
    }

    bytes1[1] _storageRef;
    // Specification of data locations for function parameters was optional before <0.5.0 but should
    // always be stated explicitly to prevent unintended assignments between different locations.
    function locationalDisorientation(bytes1[1] calldata calldataRef) external {
        // Examples of assigning between different data locations:
        bytes1[1] memory memoryRef;

        // Prior <0.5.0 uninitialized local storage variables could point to unexpected storage
        // locations in the contract, which could lead to vulnerabilities.
        // The fix was a compilation error preventing usage of uninitialized local storage variables,
        // but the issue came back under some circumstances with 0.6.5 and was fixed again with 0.8.1.
        // To prevent issues, preferably assign them immediately during declaration.
        bytes1[1] storage localStorageRef; // Bad Practice: A Storage pointer to nowhere.

        // Storage variable to local storage: Now both variables point to the same storage slot!
        localStorageRef = _storageRef;

        // Storage variable to memory: Both lines copy the contents from storage into memory.
        memoryRef = localStorageRef;
        memoryRef = _storageRef;

        // A memory reference can't become a local storage reference.
        // localStorageRef = memoryRef; // Nope!
        // But you can copy a memory value directly into storage.
        _storageRef = memoryRef; // Copies the contents from memory to storage.

        // Calldata can be copied to memory or storage (but not using local storage variable).
        // localStorageRef = calldataRef;  // Nope!
        _storageRef = calldataRef;        // Works! Copies calldata contents to storage slots.
        memoryRef = calldataRef;          // Works! Copies calldata contents to memory.
    }

    function transformer(uint8 ui8, int16 i16) public pure {
        // Implicit (automatic) conversion between value-types is possible if it makes
        // sense semantically and no information is lost
        uint16 ui16 = ui8;         // Padding of higher order bits
        int32 i32 = i16;
        // Explicit conversion can lead to unexpected behavior and loss of information.
        ui8 = uint8(ui16);         // Higher order bits are cut off
        i16 = int16(i32);
        // ui16 = uint16(i32);      // Signed to unsigned with different bit-count errors.
        ui16 = uint16(uint32(i32)); // But works with one explicit conversion at a time!
        // Same rules apply to fixed-size bytes types.

        // If you need to do some explicit conversions but want to make sure no
        // information is lost, consider libraries, eg.: OpenZeppelin's SafeCast
    }

    function _literally() private pure returns (address, uint, uint, string memory, string memory, string memory, bytes20, uint32[3] memory) {
        return(
            // Hexadecimal literals between 39 and 41 digits long must pass the
            // address checksum test and are of address type.
            // Mixed-case address checksum format is defined in EIP-55.
            // Before 0.8.0 address literals were payable by default.
            0xc32337e392AF79cAa91c395D662d389Ba280862A,
            // Rationals, decimal fractions, scientific notations are supported as literals,
            // but must be implicitly convertible into the target type.
            .99999e10,
            // Underscores and denominations should be used for readability when possible
            1_000_000_000 ether, // == 1000000000000000000000000000
            // Strings can only contain printable ascii characters
            "foo",     // Usually double quotes preferred
            'bar\r\n', // Some escape sequences are also supported since 0.7.0
            // Unicode (UTF-8) literals (were separated from normal strings in 0.7.0)
            unicode"hello😂",
            // Arbitrary Hexadecimal literals.
            // Underscores allowed between bytes, but not between nibbles (ie. 0xf_f is not allowed).
            hex"c3_2337e392AF79cAa91c395D662d389Ba280862a",
            // Array literals are static ([3]) and typed by their first element.
            // Following elements must be implicitly convertible to that type.
            [1, 0x02, .3e10]
        );
    }

    // Function Types allow for some functional programming.
    // They can only be explicitly Internal or External, which changes how they work internally.
    // Internal functions can only be called inside the current contract and their actual value
    // is a pointer to the offset of the function's code within the contract's bytecode.
    struct InternalFunc { function () internal f; }
    function jumper(uint jumpTo) public {
        InternalFunc memory func;
        // Function type variables should be carefully handled and avoided in assembly
        // manipulations to prevent jumps to arbitrary code locations.
        // The following allows jumping to any JUMPDEST opcodes within the contract's
        // bytecode, potentially jumping over important checks/requires.
        assembly { mstore(func, jumpTo) }
        func.f();
    }
    // External functions actually consist of an address and a function signature and they can
    // be passed via and returned from external function calls.
    function() external payable _paymentCallback = this.payMe;
    function setPaymentCallback(function() external payable paymentCallback_) external {
        _paymentCallback = paymentCallback_;
    }
    function getPaymentCallback() external view returns (function() external payable) {
        return _paymentCallback;
    }

    // Denominations for literal numbers.
    function whatsThatInSeconds(uint s, uint m, uint h, uint d, uint y) public pure returns (uint result) {
        result = (s * 1 seconds)
               + (m * 1 minutes)
               + (h * 1 hours)
               + (d * 1 days)    // Not every day has 24 hours!
            // + (y * 1 years);   // Was deprecated because of leap seconds!
               + (y * 365 days); // Not every year has 365 days ^!
    }
    function whatsThatInWei(uint w, uint g, uint e) public pure returns (uint result) {
        result = (w * 1 wei)
               + (g * 1 gwei)   // equals (g * 1e9) since 0.7.0
               + (e * 1 ether); // equals (g * 1e18)
        // finney and szabo denominations were removed in 0.7.0
    }


    function hashMe(uint8 num) public view returns (bytes32, bytes32, bytes32, bytes20) {
        // Hashing encodePacked can be dangerous because packing can be ambiguous!
        // Packed means no padding, so one variable can bleed into another allowing
        // for unintended collisions when dynamic-size types are used as inputs.
        return (
            // The originally proposed SHA3 algo.
            keccak256(abi.encodePacked(
                address(msg.sender),
                num
            )),
            keccak256(abi.encodePacked(
                address(msg.sender),
                // Malleability risk: Types that do not occupy the full 32 bytes might contain
                // “dirty higher order bits” which does not affect operation on types but gives
                // different results with msg.data.
                msg.data
                // For example num variable will be 1 for both msg.data 0xff000001 and 0x00000001
                // but the hashed value of msg.data will differ!
            )),
            // The official SHA3 (rumored to potentially have backdoors)
            sha256(abi.encodePacked(
                address(msg.sender),
                num
            )),
            // Legacy hashing algorithm from BitCoin times.
            // Similar to SHA1, not considered safe.
            ripemd160(abi.encodePacked(
                address(msg.sender),
                num
            ))
        );
        // Cryptography is hard to get right! Use established libraries (eg. OZ) for more complicated stuff.
    }

    error InsufficientPaymentError(uint actual, uint expected);
    function payWall() external payable {
        // Assert for checking conditions that should never fail.
        // Causes Panic Error & Revert. Before 0.8.0 consumed all gas with "invalid" opcode.
        assert(msg.value >= 0);
        // Require for checking input or external return values.
        // Causes Error & Revert.
        require(msg.value == 0 wei);
        require(msg.value < 5 wei, "More!");
        // Revert with optional reason.
        if (msg.value < 6 wei) revert();
        if (msg.value < 7 wei) revert("Mooore!");
        // Better: Named / Custom Errors
        // Is encoded into 4 bytes (same as function signatures), cheaper than a message strings.
        // Make use of NatSpec for longer error descriptions.
        if (msg.value < 8 wei) revert InsufficientPaymentError(msg.value, 8);
        // Since 0.8.26 require() supports Custom Errors when via-ir pipeline is used.
        // General support, including for the legacy pipeline was added with 0.8.27.
        require(msg.value >= 8 wei, InsufficientPaymentError(msg.value, 8));
        //      ^^^^^^^^^^^^^^^^^^ When switching between using revert() and require() always remember
        //                         that the logic of the condition is properly inverted. Common security issue!

        pubInt += msg.value;

        // You might be aware of the CEI (Checks-Effects-Interactions) pattern which should be followed
        // in order to prevent unsafe external calls from becoming reentrancy vulnerabilities.
        // Recently there's been talk about a new pattern: FREI-PI
        // Function Requirements - Effects - Interactions + Protocol Invariants
        // So in addition to the CEI pattern you may want to consider adding require() or assert() statements
        // at the end of your functions that ensure that before exiting and allowing the transaction to succeed
        // your protocol is still in a state that is expected.
        require(address(this).balance >= pubInt);
        // Note that the true FREI-PI pattern is actually defined as basically having multi-call functions
        // with checks at the beginning (input checks) and the end (invariant checks) of execution.
    }

    // To destruct or not to destruct.
    function implode() external onlyOwner {
        selfdestruct(payable(msg.sender));
        // The SELFDESTRUCT opcode was declared as deprecated in Q1 2023 with the Shanghai Upgrade.
        // Appropriately, Solidity 0.8.18 added a deprecation warning when compiling code using it.
        // But at this point it still had its original effects:
        //   - Sent all of the executing contract's ETH holding to the specified address.
        //     This balance was simply added to the recipient address without executing any code.
        //     Which means that if the recipient is a contract, it cannot react and therefore not reject ETH.
        //   - Marked the executing contract to be deleted at the end of the current (successful) transaction.
        // In Q1 2024 with the Cancun Upgrade the behavior of the SELFDESTRUCT opcode has changed:
        //   - Still sends all ETH to recipient, same as before.
        //   - The account, and therefore the contract's code, is no longer marked for deletion,
        //     EXCEPT if that contract was created within the same transaction that it's being destructed.
        // Reason for this change was the fact that deleting so much information all at once from the
        // blockchain's state was a very expensive operation and that it was preventing the introduction
        // of Verkle Trees.
    }
    address public owner;
    modifier onlyOwner() {
        // Use of tx.origin for authentication may be abused by a MITM malicious contract forwarding
        // calls from the legitimate user who interacts with it. Use msg.sender for authentication!
        require(tx.origin == owner); // Bad Practice! Use with caution.
        _;
    }

    // Stack Too Deep Error
    //
    // Note that since Solidity 0.8.13, using the new via-ir pipeline together with the optimizer
    // can fix this issue without any changes to the code. If necessary it can even move value
    // type variable values into memory to ensure the remain accessible.
    //
    // Each of the local variables get pushed on the stack in the order they are declared
    // [ u0, u1, u2, ..., u15, u16 ] <- top of the stack, containing 17 items
    function stackingTooDeep() public pure returns (uint8 u0) {
        u0 = 1;
        uint8 u1 = u0 + 1;
        uint8 u2 = u1 + 1;
        uint8 u3 = u2 + 1;
        uint8 u4 = u3 + 1;
        uint8 u5 = u4 + 1;
        uint8 u6 = u5 + 1;
        uint8 u7 = u6 + 1;
        uint8 u8 = u7 + 1;
        uint8 u9 = u8 + 1;
        uint8 u10 = u9 + 1;
        uint8 u11 = u10 + 1;
        uint8 u12 = u11 + 1;
        uint8 u13 = u12 + 1;
        uint8 u14 = u13 + 1;
        uint8 u15 = u14 + 1;
        uint8 u16 = u15 + 1;
        //u0 = u16; // CompilerError: Stack too deep, because u0 is unreachably deep
        u1 = u16; // This works, because u1 is just barely reachable.
        // The EVM only has opcodes to manipulate the top 16 elements on the stack:
        //   SWAP1, SWAP2, SWAP3 ... SWAP16

        // If we don't want to loose access to earlier declared local variables due
        // to this restriction, we can make use of scopes:
        {
            uint8 temp0 = 42;
            uint8 temp1 = 24;
            // u1 = temp0 + temp1; // We lost access to u1 within this scope.
            u16 = temp0 + temp1;
        } // At the end of this scope, values of temp0 and temp1 are popped off the
        // stack. If they wouldn't have been within the scope we'd have gotten a
        // stack to deep error again on the following line:
        u1 = u16;
    }

    // Note that, with Account Abstraction promoting the use of Smart Contract Wallets,
    // it's now generally seen as a Bad Practice to prevent other contracts from interacting
    // with your protocol.
    // Only put restrictions against calls from other contracts in place if you have a good reason!
    function isContract(address param) public view returns (bool callerIsContract, bool paramIsContract) {
        // We can reliably check whether the caller is a contract by comparing:
        callerIsContract = tx.origin != msg.sender;
        // tx.origin will always contain the address of the EOA account that originally triggered
        // the transaction (and only EOAs can trigger transactions!)

        // We can try to check the size of an accounts code to check whether it's contract:
        uint256 size;
        assembly {
            size := extcodesize(param)
        }
        paramIsContract = size > 0;
        // But this is not reliable since the size is not only 0 for EOAs but also
        //  - a contract in construction
        //  - an address where a contract will be created in the future
        //  - an address where a contract lived, but was destroyed (no longer possible since Cancun Upgrade)
    }

    // If possible the amount of work done (gas used) within a function should be relatively constant and
    // predictable, no matter the function input. Parameter and return values should also be of a constant
    // or predictable size (eg. arrays with a specific amount of items). Test the extreme cases!
    function loooooping(uint8[] calldata numbers) external {
        for (uint8 i; i < numbers.length; i++) {
          // Operations such as storage variable updates (use SSTOREs) inside a loop cost a lot of gas, are
          // expensive and may lead to out-of-gas errors. Optimizations using local variables are preferred.
          pubInt += numbers[i];
          // Calls to external contracts inside a loop are dangerous (especially if the loop index can be
          // user-controlled) because it could lead to DoS if one of the calls reverts or execution runs out
          // of gas. Avoid calls within loops, check that loop index cannot be user-controlled or is bounded.
          payable(msg.sender).transfer(1);
        }
        // Be also careful with iterating over arrays that grow in size! It may be that your protocol works
        // for a while, but then all calls to it begin failing due to the iteration consuming all gas.
    }

    // PRNG relying on block.timestamp, now or blockhash can be influenced by miners to some extent and should be avoided.
    // block.timestamp and block.number are not good proxies (i.e. representations, not to be confused with smart contract proxy/implementation pattern) for time because of issues with synchronization, miner manipulation and changing block times.
    // TODO example "not random" & time function
}

/**
 * "Free Function" outside of contract.
 *
 * Will always be implicitly internal (no explicit visibility allowed) and their bytecode
 * will be embedded into all contracts that call them, similar to internal library functions.
 *
 * They're unable to access things outside scope, eg. state variables,
 *   unless they're passed in by reference.
 *
 * It's visible and therefore usable before its declared, eg within Test contract thanks to
 * C99 scoping rules (solc >=0.5.0). Prior, the usage of a variable before its declaration
 * (either declared later or in another scope) led to unexpected behavior.
 */
function _freeFunction(bytes32[3] storage arrrr) {
    // If you intent to work on a storage value (accessing it multiple times),
    // first copy its value into memory, work on it and then set storage once!
    bytes32[3] memory val = arrrr;
    val[0] = val[0] ^ val[1] ^ val[2] ^ val[0] ^ val[1] ^ val[2];
    arrrr[0] = val[0];
}

/**
 * A EOA transaction can always only call one function on your contract, that's expensive!
 * Consider using OpenZeppelin Multicall library to support batching multiple calls!
 */
contract Tester {

    address _testAddress;

    Test _target;

    // Often parameters are prefixed with underscore.
    // Especially when it prevents shadowing existing state vars.
    // To bypass name clashes, they're sometimes instead suffixed with _.
    constructor(address testAddress_) payable {
        _testAddress = testAddress_;
    }

    function createTest() public {
        bytes32[] memory proposalNames;

        //
        _target = new Test(proposalNames, 0);
        _testAddress = address(_target);

        // TODO
        // create with ether???
        // create2 ????
        // Consider using OpenZeppelin Create2 library instead of doing this yourself!
        // create with try catch ???
        // CREATE3
    }

    function getInterfaceInfo() public pure returns (string memory, string memory, bytes4) {
        return (
            // Name of Contract / Interface.
            type(Test).name,
            type(ITest).name,

            // Only works for real Interface types not Contracts!
            // Value is all function signatures XORed (EIP-165), no inheritance!
            type(ITest).interfaceId

            // This can be used in inline assembly to build custom creation routines, especially by using the CREATE2 opcode.
            // This property cannot be accessed in the contract itself or any derived contract
            // type(Test).creationCode,

            // If C has a constructor that uses inline assembly, this might be different from the actually deployed bytecode.
            // Also note that libraries modify their runtime bytecode at time of deployment to guard against regular calls
            // type(Test).runtimeCode
        );
    }

    // TODO
    // When interacting with addresses, consider using the OpenZeppelin Address library that ensures
    // safe usage of typical interaction such as calling functions or sending ether.
    function makeCalls() public {
        // call()
        // delegatecall() and staticcall() are used the same was as call() but you can't send ether with them
        // delegatecall only uses code of address but caller's state and context
        //    ^ both contracts should have same state vars in same order!
        // staticcall throws exception for state modifying code??? or only returns false?
        // delegatecall() or callcode() to an address controlled by the user allows execution of malicious contracts in the context of the caller’s state. Ensure trusted destination addresses for such calls.
        // high level calling

        // If the address is actually a different kind of contract than expected,
        // it can still succeed if the same function exists there – don't rely on contract type checking!

        // try catching???  (introduced in 0.6.0)
        // Named errors are cheaper than comparing the reason strings

        // The low-level functions call, delegatecall and staticcall return true as their first return value if the account called is non-existent, as part of the design of the EVM. Account existence must be checked prior to calling if needed.
        // address.codehash:
        // 0x0 for an empty account, a fresh address
        // 0xc5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470 for EOAs, or contracts with no code yet / anymore


        // How would you simulate a call to b() from your contract A, observe the side effects, and then undo them entirely without reverting the main execution thread?
        // try B.b() {
        // tests
        // revert();
        // } catch {}

        // show usage of
        // abi.decode(bytes memory encodedData, (...)) returns (...): ABI-decodes the given data, while the types are given in parentheses as second argument.
        // abi.encode(...) returns (bytes memory): ABI-encodes the given arguments
        // abi.encodeWithSelector(bytes4 selector, ...) returns (bytes memory): ABI-encodes the given arguments starting from the second and prepends the given four-byte selector
        // abi.encodeWithSignature(string memory signature, ...) returns (bytes memory): Equivalent to abi.encodeWithSelector(bytes4(keccak256(bytes(signature))), …)


        // catch Error(string memory reason) { ... }: This catch clause is executed if the error was caused by revert("reasonString") or require(false, "reasonString") (or an internal error that causes such an exception).
        // Sources of Error() exceptions
        // - Calling require with an argument that evaluates to false.
        // - If you perform an external function call targeting a contract that contains no code
        // - If your contract receives Ether via a public function without payable modifier (including the constructor and the fallback function)
        // - If your contract receives Ether via a public getter function


        // catch Panic(uint errorCode) { ... }: If the error was caused by a panic, i.e. by a failing assert, division by zero, invalid array access, arithmetic overflow and others, this catch clause will be run.
        // Panic exception error codes
        // 0x01: If you call assert with an argument that evaluates to false.
        // 0x11: If an arithmetic operation results in underflow or overflow outside of an unchecked { ... } block.
        // 0x12; If you divide or modulo by zero (e.g. 5 / 0 or 23 % 0).
        // 0x21: If you convert a value that is too big or negative into an enum type.
        // 0x22: If you access a storage byte array that is incorrectly encoded.
        // 0x31: If you call .pop() on an empty array.
        // 0x32: If you access an array, bytesN or an array slice at an out-of-bounds or negative index (i.e. x[i] where i >= x.length or i < 0).
        // 0x41: If you allocate too much memory or create an array that is too large.
        // 0x51: If you call a zero-initialized variable of internal function type.

        // catch (bytes memory lowLevelData) { ... }: This clause is executed if the error signature does not match any other clause, if there was an error while decoding the error message, or if no error data was provided with the exception. The declared variable provides access to the low-level error data in that case.

        // catch { ... }: If you are not interested in the error data, you can just use catch { ... } (even as the only catch clause) instead of the previous clause.

        // If execution reaches a catch-block, then the state-changing effects of the external call have been reverted.
        // The caller always retains 1/64th of the gas in a call and thus even if the called contract goes out of gas (consumes 63/64 of all gas), the caller still has some gas left

        // abi.encodeCall() was added with 0.8.11 and had a bug until 0.8.13 where hex and string literals directly passed as arguments would be incorrectly encoded with implicitly compatible types (eg. uint16 instead of bytes2, which have differing alignments)
    }

    /**
     * To avoid reentrancy attacks when sending ether, make sure to use a guard,
     * for example OpenZeppelin's well tested ReentrancyGuard.
     *
     * Alternatively you can make sure to follow the Checks Effects Interactions (CEI)
     * pattern, basically: Make sure to update the state first and make the send last.
     *
     * Additionally it's recommended to make use of the pull-payment strategy, this
     * is especially relevant if you need to pay out ether to multiple addresses.
     * For this there's as well the OpenZeppelin PullPayment lib.
     **/
    function sendEther() public payable {
        // Only an address that is made payable has transfer() and send() members.
        // Implicit conversion to payable was removed in 0.8.0
        // To address conversion from compatible types: uint160, bytes20 and contract types
        address payable ptestAddress = payable(_testAddress);

        // These only send 2300 gas! ("to prevent reentry")
        // Usage no longer recommended since EIP1884 increases the gas cost of certain opcodes, possibly making
        // contracts go over the 2300 gas limit imposed by transfer, making them unable to receive funds via transfer.
        // Only use them if you're certain the receiver will be an EOA!
        ptestAddress.transfer(1 wei); // Exception & Revert on failure! (eg. not enough balance, OOG Out of Gas)
        // Low-level counterpart (return values instead of throwing errors)
        // ptestAddress.send(1 wei); // Only send itself reverts on failure!

        // You should use call() instead, without hardcoded gas limits along with checks-effects-interactions pattern
        // or reentrancy guards (eg. OpenZeppelin's ReentrancyGuard) for reentrancy protection.
        // Low-level call funcs (using opcodes, allows calling non-ABI compatible addresses):
        (bool success,) = _testAddress.call{ value: msg.value, gas: 2300 }("");
        //           ^                    ^ {} notation since 0.7.0, before: .value(msg.value).gas(2300)
        //           ^ Return value deconstruction with skipping by omission.
        if (!success) revert();

        // High Level calling other contracts also allows sending to external payable functions.
        // _target.payMe{value: 1 wei}();
        // Because the Test Interface/Contract has payable functions, the address to cast from must be payable too
        ITest test = ITest(ptestAddress);
        test.payMe{value: 1 wei}();
    }
}

/**
 * Libraries have no state or balance and do not support inheritance.
 *
 * Embedded Library: If a smart contract is consuming a library which has only internal functions,
 * then the EVM simply embeds the library into the contract. Instead of using delegate call to call a
 * function, it simply uses a JUMP statement (internal method call). Same as free internal function are embedded.
 *
 * Linked Library: If a library contains public or external functions then the library needs to be deployed.
 * The deployment of the library will generate a unique address of the library contract on the blockchain.
 * This address will be "linked" with the delegate-calling contract that makes use of the library.
 *
 * (CALLCODE instead of DELEGATECALL was used until Homestead, version 0.4.20 - msg.sender, value, etc - changed!)
 */
library UnsafeMath {

    /**
     * A library can be attached to a type: "using UnsafeMath for Price;"
     * These functions will receive the object they are called on as their first parameter:
     * "somePrice.sub(otherPrice)" is equivalent to "UnsafeMath.sub(somePrice, otherPrice)"
     *
     * External/Public function of linked libraries can only be called via DELEGATECALL.
     * This is an automatic protection to prevent killing libraries by calling a function
     * containing SELFDESTRUCT() which would brick contract using the library.
     */
    function sub(Test.Price a, Test.Price b) public pure returns (Test.Price) {
        // Before 0.8.0 a SafeMath library should have been used, now its save
        //   by default since it'll error when it wraps (checked mode).
        // Old overflowing behavior still available in unchecked mode:
        unchecked {
            // When working with User Defined Value Types, before we can work with the
            // underlying value, we first have to unwrap it, then apply our actions, and
            // finally wrap it up in the desired type again.
            return Test.Price.wrap(Test.Price.unwrap(a) - Test.Price.unwrap(b));
        }
    }

    function add(Test.Price a, Test.Price b) public pure returns (Test.Price) {
        unchecked {
            return Test.Price.wrap(Test.Price.unwrap(a) + Test.Price.unwrap(b));
        }
    }

}
