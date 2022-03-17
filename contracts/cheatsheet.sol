// SPDX-License-Identifier: GPL-3.0
// ^ recommended, included machine readable in bytecode metadata
// Software Package Data Exchange is an open standard

pragma solidity ^0.8.13;
// ^ floating pragma, min 0.8.13 max excluding 0.9.0
// same as complex pragma: pragma solidity >=0.8.13 <0.9.0;
// major.breakingchanges.bugfixes
// only makes the compiler check for compatibility and throws error if not matching!
// should only be floating during development, fixed everywhere during testing & deployment
// consider using a current but well tested version (latest might have undiscovered bugs)

pragma abicoder v2;
// ^ v2 able to en/decode arbitrary nested arrays and structs, v2 is default since 0.8.0
// contracts using v2 are backwards compatible with v1, as long as v1 contract doesn't
// try to call a v2 contract function that needs the new features (compiler errors)
// v1 contracts are allowed to inherit from v2 contracts as long as its only used internally

// Remix also allows imports from GitHub via URL but without the protocol.
// Create a new file contracts/Test.sol in Remix in order to compile and play around!
import "./1_Storage.sol";
// ^ imports its globals into this global scope
import * as ownerNamespace from "./2_Owner.sol";
// import "./2_Owner.sol" as ownerNamespace;
// ^ imports its globals into a namespace without polluting this global scope
import { Ballot } from "./3_Ballot.sol";
//import { Ballot as BallotContract } from "./3_Ballot.sol";
// ^ import specific global symbols into this global scope, optionally renaming it

// Interfaces are usually prefixed with an I.
// May only inherit from other interfaces.
// They can't have state vars or constructors.
interface ITest {
    // Even if they're actually public, in an Interface all functions must be external.
    // Functions in interfaces are implicitly virtual.
    function payMe() external payable;
}

// Abstract Contracts may be missing the body of one or more functions (to be implemented by a child).
// abstract and enforced usage of virtual/override keywords was introduced in 0.6.0
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
    // If we don't mark it as virtual again the inheriting Contract may not override it.
    // When overriding functions: External may become public, mutability may become stricter (if not payable).
    function nobody() public pure override {
        // nop.
    }

    // Function is overriden but kept virtual for further overrides.
    function _overrideMePlease(uint a) internal pure override virtual returns (uint) {
        // A Base Contract implementation, one level higher up in the flattened inheritance hierarchy,
        // can be called using super. Alternatively a specific Base Contract can be specified.
        return super._overrideMePlease(a) ** 2;
        // Equivalent: TestBase._overrideMePlease()
    }

    // When overriding a function or modifier defined in multiple parallel bases, all bases must be listed:
    modifier checker() override(TestBase, TestBase2) {
        _;
    }
}

/**
 * @title Test, a Contract similar to a Class in OO languages
 * @author patrickd
 * @notice NatSpec example – a notice for endusers, becomes part of JSON ABI to be used in UIs
 * @dev Stands for Ethereum Natural Language Specification Format
 * @custom:example starts with custom, ends with anything, specific to application
 *
 * This is this file's main Contract. The best practice is to give the file the name of this
 * contract: Test.sol – In the best case a file should only contain a single contract.
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

    // TODO refactor to follow best practices regarding ordering of elements within contract/file
    // Pragma statements, Import statements, Interfaces, Libraries, Contracts. Inside each contract,
    // library or interface, use the following order: Type declarations, State variables, Events, Functions
    // Order of Functions: Ordering helps readers identify which functions they can call and to find the
    // constructor and fallback definitions easier. Functions should be grouped according to their
    // visibility and ordered: constructor, receive function (if exists), fallback function (if exists),
    // external, public, internal, private. Within a grouping, place the view and pure functions last.

    // Statically-typed! Means all types must be known at compile time.
    // There's no concept of null or undefined.
    // All "empty" variables are well defined in "zero-state" – it's preferrable to explicitly
    // assign them a value to avoid incorrect assumptions or unintended use of zero-addresses.
    // An exception to this rule are upgradable contracts where the constructor wouldn't be executed in
    // the proxy's context and initialize functions are used instead – assign values in initialize()!
    uint public pubInt = 0;         // Automatically has a public getter func of same name.
                                    // Only public state vars can have NatSpec comments (since 0.7.0)
    bool internal _intBool = false; // Can only be used from this or inheriting contracts.

    uint internal _mutex = 1;   // See reentrancyGuard modifier.

    bytes32[3] private _privA; // contains 3, 32 bytes long bytearrays filled with zeros,
                               //   because it's statically sized!
                               // Can only be used within this Test contract (private),
                               //   don't forget that all data on the blockchain is actually public.
                               // Arrays (also bytes and strings) are reference types!

    uint8 public constant CONST = 42; // Is placed everywhere it's used during compile time.
    uint8 internal immutable _immu;   // Becomes immutable after being set during construction.
                                      // Is placed into 32 byte placeholder slots when set,
                                      // meaning in this 8bit case the constant is cheaper!
                                      // Both are placed directly into the runtime bytecode
                                      // that means no storage is used!

    // Only value types are supported, but constant supports strings too!
    string private constant _FOOBAR = "foobar!";

    // Depending on their order, state variables can be packed into the same slot (starting at slot 0).
    // Packing reduces storage slot usage but increases opcodes necessary to read/write to them.
    uint248 _right; // 31 bytes, Doesn't fit into the previous slot, get's a new one
    uint8   _left;  // 1 byte, There's still 8 free bits in the 256 bit slot
    // ^ one storage slot will be packed from right to left with these two values (lower-order aligned)
    // Structs and array data always start a new slot!
    // Dynamically-sized arrays use their slot p for the length, their values start being stored at keccak(p)
    // Mappings leave their slot p unused (to avoid clashes), their individual values are stored at
    //   keccak(h(k) + p) with h() padding value types or hashing reference types.
    // Bytes and Strings larger than 31 bytes work like arrays, but when smaller, the bytes of smaller once
    //   are stored in the higher order bits of their slot together with the length value.
    // Same rules apply for inherited state variables, they are as if "merged" in their C3-linearized
    //   order, starting with the most base-ward contract.
    // Note there's no packing in memory! Memory is always padded to 32 bytes, and no GC.

    /*
     * Non-NatSpec comment. Use NatSpec at least on all public interfaces!
     * It'll be included in the ABI JSON information to be used by your and
     * potentially other application user interfaces.
     */

    /**
     * The constructors of all the base contracts will be called following the linearization rules,
     * starting with the most base-ward Contract: ITest, AdvTestBase, ..., Ballot, Test
     *
     * If the base constructors have arguments, derived contracts need to specify all of them either
     * in the inheritance list or here in the derived constructor. If the base contracts don't have
     * a constructor implemented you shouldn't call them to prevent misplaced assumptions.
     *
     * Is executed once with a transaction, generating the runtime bytecode which is then actually
     * deployed. All internal functions only called by the constructor are not included in the
     * runtime bytecode.
     *
     * Until solc <0.5.0, the constructor could also be the name of the class instead of using the
     * constructor keyword. If that "named constructor" was not exactly matching the class name
     * it would end up as a public function in the runtime bytecode, a potential vulnerability!
     */
    constructor(bytes32[] memory proposalNames, uint8 immuV) Ballot(proposalNames) {
        _immu = immuV + 2; // IMMU is write-only during construction and read-only afterwards
                            // >=0.8.8 allows reading here too

        // Inline assembly is a way to access the Ethereum Virtual Machine at a low level.
        // The language used for inline assembly is called Yul which has improved readability.
        // Use of EVM assembly is error-prone and should be avoided or double-checked for correctness.
        assembly {
            // Yul parses comments, literals and identifiers in the same way as Solidity.
            // You can access Solidity variables and other identifiers by using their name.
            // Local variables of value type are directly usable in inline assembly
            let numba := immuV // < No ; semicolons!
            //^       ^ assignments with ":=" instead of "="!
            //^ Uses the "u256" type, which is the native 256-bit type of the EVM.

            // Local variables that refer to memory/calldata evaluate to the address of the
            // variable in memory/calldata and not the value itself!
            let numberOfProposals := mload(proposalNames) // Elements in memory arrays in Solidity
                                                          // always occupy multiples of 32 bytes.
                                                          // The length of a dynamic array is stored
                                                          // at the first slot of the array and
            let proposal1 := mload(add(proposalNames, 32))// followed by the array elements

            // For local storage variables or state variables, a single Yul identifier is not sufficient,
            // since they do not necessarily occupy a single full storage slot. Therefore, their “address”
            // is composed of a slot and a byte-offset inside that slot.
            let rightAndLeftValues := sload(_left.slot)
            // let leftValue := rightAndLeftValues[_left.offset] TODO how?

            // You can assign to the .slot part of a local storage variable pointer.
            // For these (structs, arrays or mappings), the .offset part is always zero. It is not
            // possible to assign to the .slot or .offset part of a state variable, though


            // Reserved Memory: Solidity reserves four 32-byte slots, with specific byte ranges
            // (inclusive of endpoints) being used as follows:
            // 0x00 - 0x3f (64 bytes): scratch space for hashing methods
            // 0x40 - 0x5f (32 bytes): currently allocated memory size (aka. free memory pointer)
            // 0x60 - 0x7f (32 bytes): zero slot (The zero slot is used as initial value for dynamic
            //                         memory arrays and should never be written to)
            // There is no guarantee that the memory has not been used before and thus you cannot
            // assume that its contents are zero bytes. There is no built-in mechanism to release
            // or free allocated memory.
            // TODO example using new memory and increasing pointer

            // TODO
            // if statements, e.g. if lt(a, b) { sstore(0, 1) }
            // switch statements, e.g. switch mload(0) case 0 { revert() } default { mstore(0, 1) }
            // for loops, e.g. for { let i := 0} lt(i, 10) { i := add(i, 1) } { mstore(i, 7) }
            // function definitions, e.g. function f(a, b) -> c { c := add(a, b) }


            // Shift operators (shl(x, y), shr(x, y), sar(x, y)) in Solidity assembly apply the
            // shift operation of x bits on y and not the other way around, which may be confusing.
            // Make sure you have the parameters in the right order!
        }
    }

    /**
     * @notice returns what you pass
     * @dev echo
     * @param test - can be anything. function parameters are like local vars and can be reassigned
     * @return will return test. is unnamed here and needs to be returned with return keyword
     *
     * Public means it can be called externally AND internally!
     * Since they can be called by ANYONE, make sure that you have appropriate access control!
     * If the function will not be called internally, consider making it external to save gas.
     *
     * Prioar to <0.5.0 specifying a visibility was optional and the default was public causing
     * vulnerabilities from incorrect visibility assumptions.
     */
    function echo(uint test) public pure returns (uint) {
        // Shadowing of state variables within functions only causes a warning but should be avoided.
        // uint pubInt;
        // A common practice is to pre or append underscores to such variables:
        uint _pubInt = test;

        /// @notice single line NatSpec comment with 3 slashes, while blocks have two asterisks
        return _pubInt;
    }

    /**
     * Return variables can be pre-named and set like normal local variables.
     * Will be initialized with default zero-state.
     * No return statement necessary!
     *
     * We're also using Function Overloading here! Same name different params!
     *
     * Since uint8 is a value type, no data location can be specified!
     *
     * External means it can only be called via messages! (Other contracts/transactions)
     * Or, by the same contract, using `this.echo(uint8(1))`!
     * External is preferrable over public since it can use calldata data location.
     */
    function echo(uint8 test) external pure returns (uint8 testr) {
        testr = test;
        // There's no implicit type conversion to booleans!
        // So for conditionals we have to make explicit comparisons that result in booleans.
        if (test == 1) {
            return testr; // If we WANT to use return, we MUST specify the return value anyway;
        }
    }

    /// @dev example with multiple return values
    // A whitespace separated list of modifiers can be applied to functions.
    // Modifiers that don't have parameters are called without "()".
    function echo(uint16 test) public freeChecker paramCheck(test) returns (uint8 test8, uint16 test16, uint32) {
        test16 = test;
        test8 = uint8(test); // explicit conversion
        return ( // count and types must match!
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
     */
    modifier freeChecker {
        _freeChecker();
        _; // Continue executing the function body using this modifier.
        _; // Body can be skipped, aborted or executed multiple times.
        if (false) _; // When skipped the return values will stay in previous/default state.
                      // Modifiers that don't execute the function body and return default
                      // values, may cause unexpected behavior!
    }
    function _freeChecker() private {
        freeeeeee(_privA);
    }

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
     * View functions are executed using the STATICCALL opcode that ensures they
     * can only read blockchain state and not write to it.
     *
     * Prior to 0.5.0 no STATICCALL was used and state changes could be made
     * stealthily within inline assembly.
     */
    function noTouching() public view returns (uint) {
        // pubInt++; nope!
        return block.number;
    }

    /**
     * Pure functions may only access CALLDATA
     *
     * Pure doesn't allow access read access either but isn't enforced by the EVM.
     */
    function borderlinePure() public pure returns (bytes calldata, bytes4) {
        return (
            msg.data,
            // msg.sender, nope!
            // msg.value, nope!
            // tx.gasprice, nope!
            msg.sig
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
     * filter for specific events when subscribing. (Topics are stored as hashes)
     * Non-Indexed topics go into the data part of that log, can't be filtered.
     */
    event ChingChing(address indexed sender, uint moneyz);

    /**
     * If you have a function that will be called very often, in order to safe gas..
     *  - declare functions as external and make use of calldata location for parameters.
     *    This will avoid expensive memory copying operations when the function is called.
     *  - declare external functions ordered by how likely they are to be called, most to least.
     *    The order influences how fast the function selector will be able to pick the called function.
     *  - declare functions as payable in order to omit the value-is-zero check that is added by default.
     *    Make sure there's a way to withdraw unintentionally sent ether though.
     *  - name the function so its signature has many zeros as a prefix (deposit_CIx() = 0x00007693)
     *    Tool: https://emn178.github.io/solidity-optimize-name/ - make sure you have no func sig clashes!
     *  - have it make use of events instead of storage as much as possible.
     */
    function deposit_CIx() external payable { // solhint-disable-line func-name-mixedcase
        emit ChingChing(msg.sender, msg.value);
    }

    /**
     * When delete is applied on a struct, all its elemts are reset to zero-state,
     * except mappings!
     *
     * Structs and array data always have their own slot and their items are packed tightly
     *
     * Is a reference type!
     */
    struct MagicGroupOfVariables {
        uint unsignedFun;
        address whereDoesItPoint;
        Hat magicHat; // Can contain other structs but not itself!
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
    // Default / zero state is 0, first element.
    // Is a value type despite looking complex!
    enum Colors {
        Red,
        Green,
        Blue
    }

    function _doMagic() private pure {
        // Since this is a reference type, we need to specify a storage location!
        MagicGroupOfVariables memory g1; // Initialized with defaults by declaration.
        g1.whereDoesItPoint = address(0x0); // Zero values can more freely be cast into addresses and bytearrays
                                            // Prevent unintended burning by disallowing zero addresses as destinations!
        g1.magicHat.containsRabbit = true; // Access members with dot notation.
        g1.poof = Colors.Blue;

        // Struct initialization possible with specific values.
        Hat memory h1 = Hat(true);
        Hat memory h2 = Hat({ containsRabbit: true });
        delete h1; // Resets to default values
        delete h2; // (has no effect on contained mappings)
    }

    /**
     * Must be declared exactly like this, no function keyword, no params, no return!
     *
     * Executed on empty CALLDATA (usually by send() / transfer() calls)
     * Only receives 2300 gas from those, barely enough for some logging.
     *
     * No receive? Can still receive!
     * - any other payable functions, obviously.
     * - from selfdestruct() – which does no calls when sending ether
     * - as block reward benificiary (coinbase)
     * - is currently under construction
     *
     * If you have payable functions make sure that there's also a way to withdraw
     * the ether, otherwise it might be stuck forever!
     *
     * Exists since 0.6.0, before there was the unnamed fallback function:
     *   function() public payable {}
     */
    receive() external payable {
        emit ChingChing(msg.sender, msg.value);
    }

    // Similar to receive.
    // Payable and calldata parameter and return value are optional though.
    // Executed when no function matches signature.
    // Also executed on empty CALLDATA but only if there's no receive().
    // Could also only receive 2300 gas, if there's no receive().
    fallback(bytes calldata _input) external payable returns (bytes memory _output) {
        if (msg.value > 0) emit ChingChing(msg.sender, msg.value);

        // Array slicing (index range) is only available for dynamic calldata arrays: (since 0.6.0)
        bytes memory slicinAround = _input[:]; // Full copy with [0:_input.length] defaults
        slicinAround = _input[0:4]; // Function 4-byte-signature from calldata (same as msg.sig)
        slicinAround = _input[4:];  // Function argument data (all bytes after 4-byte-signature)
        // Array slices are unnamed types with no members.
        // Are scoped references to the underlying array (relative index access).

        return slicinAround;
    }

    // Functions that implement an Interface function are also "overriding".
    function payMe() external payable override {
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
     * Known items are deletable via keys but deleting the map has no effect.
     * Values can be of any types and all values are well in zero-state.
     *
     * If you need more control over the mapping consider using OpenZeppelin's
     * EnumerableMap or EnumerableSet libraries!
     */
    mapping(address => MagicGroupOfVariables) _magicians;

    function boolingAround(bool a, bool b, bool c) public pure returns (bool) {
        // Short-circuit rule: it won't evaluate/check the rest of the variables
        //   when the result is already determined by previous variables
        return a && (b || c);
    }


    // Integers come in sizes between 8 to 256 bit, in steps of 8.
    // Aside from obvious math arithmetic operators, it can do
    // Bitwise & (AND) | (OR) ^ (XOR) ~ (NEG)
    // Shift << (left) >> (right)
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

    // Apply library to a type, but only within this contract (inheritable pre 0.7.0).
    using UnsafeMath for uint8; // Works for all types! Can't be done in functions!
    // Since 0.8.13 this can be used
    //   - at file level and then applied gobally for user defined types
    //   - with individual library functions and free functions
    // eg.:
    //   type uuint8 is uint8;
    //   using { UnsafeMath.sub, UnsafeMath.add } for uuint8 global;

    function unsafeMath(uint8 a, uint8 b) public pure returns (uint8 subbed, uint8 diffedUp, uint8 diffedRight) {
        // Results from libraries are returned and not applied to the value!
        subbed = a.sub(b); // Calls sub(a, b) on the UnsafeMath library
        // If you are looking for more math functions that are typically provided
        // look for libraries such as OpenZeppelin Math (max(), min(), avg()).

        // Performing multiplication before division is generally better to avoid
        // oss of precision because Solidity integer division might truncate.
        diffedUp = (a / 255) * b;                        // (7 / 255) * 255 = 0
        diffedRight = uint8((uint(a) * uint(b)) / 255);  // (7 * 255) / 255 = 7
    }


    // Shifty business?????
    // Space efficient boolean array with bitmaps in uintX
    // Read: bitmap & (1 << index) > 0
    // Use bitwise OR to set true, AND with XOR'ed mask to set false, XOR to toggle
    // Better to use well tested libraries: OpenZeppelin BitMaps.

    /**
     * Private functions are often prefixed with underscore.
     * Since they are only available within their contract, they cannot be virtual.
     *
     * Due to a compiler bug, prior to <v0.5.17 private functions could override functions
     * in base contracts although they shouldn't be accessible at all.
     *
     * Fixed Point Number types exist but cannot be used, aside from being declared.
     */
    function _fixMe() private pure returns (ufixed nope, fixed doubleNope) {
        // Use libs! DSMath, PRBMath, ABDKMath64x64
    }

    // Prefer Fixed-size byte "arrays"!
    function _byteMe() private pure returns (bytes32, bytes1[32] memory) {
        // "byte" alias of "bytes1" was removed in 0.8.0
        bytes32 cheapByteArray;               // bytes1, 2, 3, ... 32 in a single memory slot, Value Type!
        bytes1[32] memory expensiveByteArray; // A full 32 byte memory slot is wasted for each byte
        return(cheapByteArray, expensiveByteArray);
    }

    // v type of array can be anything, even structs and maps
    uint128[3] private _sFixedArray;   // Fixed size at compile time.
    uint128[] private _sDynamicArray;  // Always reference types!
    function arraysOfFun(uint size) public returns (uint128[3] memory, uint128[] memory) {
        uint128[3] memory mFixedArray; // Fixed size at compile time.

        // Memory arrays are able to reference arrays of various sizes
        // the arrays themselves are not resizable though.
        uint128[] memory mDynamicArray = new uint128[](size);

        // Pre 0.6.0 arrays could be resized by setting length member.
        // _sDynamicArray.length = 100;

        // Only dynamic storage arrays are resizable!
        _sDynamicArray.push(); // Empty push, appends a zero-state value!
                              // Constant gas cost thanks to zero-initialization
                              // Pre 0.6.0 push() returned the new length
        _sDynamicArray.pop();  // implicit "delete" on last element
                              // Everything is re-set to zero-state, can be very costly!

        // Remove arbitrary array element (eg. first instead of last one):
        // Replace with last item and pop it off (ordering not maintained)
        _sDynamicArray[0] = _sDynamicArray[_sDynamicArray.length - 1];
        _sDynamicArray.pop();

        // Prior <v0.7.3 had a compiler bug when assigning a dynamically sized array with types of size
        // at most 16 bytes in storage causing the assigned array to shrink, some parts of deleted slots
        // were not zeroed out.
        _sDynamicArray = [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1];
        _sDynamicArray = [2];

        // Parameters and return values should always have predictable sizes to avoid DoS.
        // Also never have loops without bounds for the same reasons.
        return (mFixedArray, mDynamicArray);
    }

    // Special arrays: bytes and string – reference types!
    bytes _myBytes;   // Like bytes1[] but tightly packed (no padding).
                      // For arbitrary length raw byte data.
    string _myString; // Like bytes but no length, no [x] access, no push.
                      // Use libs for string manipulation!
                      // For arbitrary length UTF-8 strings.
    // Solidity doesn't have any string manipulation members, consider
    // looking for a library for those, eg. OpenZeppelin Strings.
    function bytesNstrings() public {
        bytes memory myMemBytes = hex"f00b43";
        string memory myMemString = "foobar";
        _myBytes = myMemBytes;
        _myString = myMemString;
        // Like arrays, only storage bytes can be resized.
        _myBytes.push(0x0);
        // _myString.push(0x0); nope for strings!
        // myMemBytes.push(0x0); nope for memory!
        // myMemString.push(0x0); nope for memory!

        // Prior <v0.7.4 had a compiler bug when copying an empty byte array (or string) from
        // memory or calldata to storage could result in data corruption if the target array's
        // length was increased subsequently without storing new data.
        bytes memory emptyBytes;
        _myBytes = emptyBytes; // Copies too much from memory to storage.
        _myBytes.push();       // Only increases length without setting storage to 0.
        // _myBytes now had whatever came after emptyBytes in memory "added" to it
    }

    function _litterally() private pure returns (address, uint, uint, string memory, string memory, string memory, bytes20, uint32[3] memory) {
        return(
            // Hexadecimal literals between 39 and 41 digits long must pass the
            // address checksum test are of address type.
            // Mixed-case address checksum format is defined in EIP-55.
            // Before 0.8.0 address literals were payable by default.
            0xc32337e392AF79cAa91c395D662d389Ba280862A,
            // Rational, decimal fraction, scientific notation
            // – all supported but must be implicitly convertable into the type
            .99999e10,
            // Underscores and denominationss should be used for readability when possible
            1_000_000_000 ether, // == 1000000000000000000000000000
            // Strings can only contain printable ascii characters
            "foo",     // Usually double quotes preferred
            'bar\r\n', // Some escape squences are also supported since 0.7.0
            // Unicode (UTF-8) literals (moved from strings in 0.7.0)
            unicode"hello😂",
            // Arbitrary Hexadecimal literals.
            // Underscores allowed between bytes, but not between nibbles (0xf_f).
            hex"c3_2337e392AF79cAa91c395D662d389Ba280862a",
            // Array literals are static ([3]) and typed by their first element (uint32)
            // Following elements must be implicitly convertable.
            [1, 0x02, .3e10]
        );
    }

    // Function Types also allow some functional programming.
    // They can only be explicitly Internal or External, which changes how they work internally.
    // Internal functions can only be called inside the current contract and their actual value
    // is a pointer to the function's bytecode.
    struct Func { function () internal f; }
    function jumper(uint jumpTo) public {
        Func memory func;
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

    bytes1[1] _storageRef;
    // Specification of data locations for function parameters was optional before <0.5.0 but should
    // always be state explicitly to prevent unintended assignments between different locations.
    function locationalDisorientation(bytes1[1] calldata calldataRef) external {
        // Examples of assigning between different data locations:
        bytes1[1] memory memoryRef;

        // Prior <0.5.0 uninitialized local storage variables could point to unexpected storage
        // locations in the contract, which could lead to vulnerabilities.
        // The fix was a compilation error preventing usage of uninitialized local storage variables,
        // but the issue came back under some circumstances with 0.6.5 and was fixed again with 0.8.1.
        // To prevent that, assign them immediately during declaration.
        bytes1[1] storage localStorageRef;

        // Storage to local storage, references the same storage.
        localStorageRef = _storageRef;

        // A storage reference to memory, makes a copy.
        memoryRef = localStorageRef;
        memoryRef = _storageRef;

        // A memory reference can't become a local storage reference.
        // localStorageRef = memoryRef;
        // But you can copy a memory value directly into storage.
        _storageRef = memoryRef;

        // Calldata can be copied to memory or real storage.
        // localStorageRef = calldataRef;
        _storageRef = calldataRef;
        memoryRef = calldataRef;
    }

    function transformer(uint8 ui8, int16 i16) public pure {
        // Implicit (automatic) conversion between value-types is possible if it makes
        // sense semantically and no information is lost
        uint16 ui16 = ui8;       // Padding of higher order bits
        int32 i32 = i16;
        // Explicit conversion can lead to unexpected behavior and loss of information.
        ui8 = uint8(ui16);       // Higher order bits are cut off
        i16 = int16(i32);
        // ui16 = uint16(i32);      // Signed to unsigned with different bit-count fails
        ui16 = uint16(uint32(i32)); // But works one conversion at a time!
        // Same rules apply to fixed-size bytes types.

        // If you need to do some explicit conversions but want to make sure no
        // information is lost, consider libraries, eg.: OpenZeppelin SafeCast
    }

    // Denominations for literal numbers.
    function whatsThatInSeconds(uint s, uint m, uint h, uint d, uint y) public pure returns (uint result) {
        result = (s * 1 seconds)
               + (m * 1 minutes)
               + (h * 1 hours)
               + (d * 1 days)    // Not every day has 24 hours!
            // + (y * 1 years);  // Was deprecated! Leap seconds!
               + (y * 365 days); // Not every year has 365 days!
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
        // for unintended collisions!
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
                // For example num will be 1 for both msg.data 0xff000001 and 0x00000001
                // but the hashed value of msg.data will differ!
            )),
            // The official SHA3 (possibly backdoored by CIA?)
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
        // Cryptography is hard to get right.
        // Use established libraries (eg. OZ) for more complicated stuff.
    }

    error InsufficientPaymentError(uint actual, uint expected);
    function payWall() external payable {
        // Check conditions that should never fail.
        // Causes Panic Error & Revert, before 0.8.0 consumed all gas with "invalid" opcode.
        assert(msg.value >= 0);
        // Check input or external return values.
        // Causes Error & Revert.
        require(msg.value == 0 wei);
        require(msg.value < 5 wei, "More!");
        // Revert with optional reason.
        if (msg.value < 6 wei) revert();
        if (msg.value < 7 wei) revert("Mooore!");
        // Better: Named / Custom Errors, but not supported by require() yet
        // Is encoded into 4 bytes, cheaper than a message string; Use NatSpec for longer description
        if (msg.value < 8 wei) revert InsufficientPaymentError(msg.value, 8);

        pubInt += msg.value;

        // Input validation is very common even from web2, but in smart contracts you should also
        // consider checking whether your return values look as expected.
        require(address(this).balance >= pubInt);
    }

    // Functions calling selfdestruct should be well protected from unauthorized calls!
    function implode() external onlyOwner {
        // Marks this contract for deletion for when the transaction was successfull.
        // Doesn't execute the receive/fallback functions of the address it sends the funds to.
        selfdestruct(payable(msg.sender));
    }
    address public owner;
    modifier onlyOwner() {
        // Use of tx.origin for authorization may be abused by a MITM malicious contract forwarding
        // calls from the legitimate user who interacts with it. Use msg.sender instead!
        require(tx.origin == owner);
        _;
    }

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
        // stack to deep error again on the next line:
        u1 = u16;
    }

    // PRNG relying on block.timestamp, now or blockhash can be influenced by miners to some extent and should be avoided.
    // block.timestamp and block.number are not good proxies (i.e. representations, not to be confused with smart contract proxy/implementation pattern) for time because of issues with synchronization, miner manipulation and changing block times.
    // TODO example "not random" & time function

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
        //  - an address where a contract will be created (CREATE2)
        //  - an address where a contract lived, but was destroyed
    }

    // If possible the amount of work done (gas used) within a function should be relatively constant and
    // predictable, no matter the function input. Parameter and return values should also be of a constant
    // or predictable size (eg. arrays with a specific amount of items). Test the extreme cases!
    function loooooping(uint8[] calldata numbers) external {
        for (uint8 i; i < numbers.length; i++) {
          // Operations such as state variable updates (use SSTOREs) inside a loop cost a lot of gas, are
          // expensive and may lead to out-of-gas errors. Optimizations using local variables are preferred.
          pubInt += numbers[i];
          // Calls to external contracts inside a loop are dangerous (especially if the loop index can be
          // user-controlled) because it could lead to DoS if one of the calls reverts or execution runs out
          // of gas. Avoid calls within loops, check that loop index cannot be user-controlled or is bounded.
          payable(msg.sender).transfer(1);
        }
    }

}

/**
 * "Free Function" outside of contract.
 * Will always be implicitly internal (no explicit visibility allowed) and bytecode will be
 * included in all contracts that call them, similar to internal library functions.
 *
 * They're unable to access things outside scope, eg. state variables, unless they're passed
 *
 * It's visible and therefore usable before its declared, eg within Test contract thanks to
 * C99 scoping rules (solc >=0.5.0). Prior the usage of a variable before its declaration
 * (either declared later or in another scope) led to unexpected behavior.
 */
function freeeeeee(bytes32[3] storage arrrr) {
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
        _target = new Test(proposalNames, 0);

        _testAddress = address(_target);

        // TODO
        // create with ether???
        // create2 ????
        // Consider using OpenZeppelin Create2 library instead of doing this yourself!
        // create with try catch ???
    }

    function getInterfaceInfo() public pure returns (string memory, string memory, bytes4) {
        return (
            // Name of Contract / Interface.
            type(Test).name,
            type(ITest).name,

            // Only works for real Interface types not Contracts!
            // Value is all function signatures XORed (EIP-165), no inheritance!
            type(ITest).interfaceId

            // This can be used in inline assembly to build custom creation routines, especially by using the create2 opcode.
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
     * To avoid reentracy attacks when sending ether, make sure to use a guard,
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
        // Address conversion compatible types: uint160, bytes20 and contract types
        address payable ptestAddress = payable(_testAddress);

        // These only send 2300 gas! ("to prevent reentry")
        // Usage no longer recommended since EIP1884 increases the gas cost of certain opcodes, possibly making
        // contracts go over the 2300 gas limit imposed by transfer, making them unable to receive funds via transfer.
        ptestAddress.transfer(1 wei); // Exception & Revert on failure! (eg. not enough balance, OOG)
        // Low-level counterpart (return values instead of throwing errors)
        //ptestAddress.send(1 wei); // Only send itself reverts on failure!

        // You should use call() instead, without hardcoded gas limits along with checks-effects-interactions pattern
        // or reentrancy guards (eg. OpenZeppelin's ReentrancyGuard) for reentrancy protection.

        // Low-level call funcs (using opcodes, allows calling non-ABI compatible addresses):
        (bool success,) = _testAddress.call{ value: msg.value, gas: 2300 }(""); // Only what happened within call reverts on failure!
        //           ^                    ^ {} notation since 0.7.0, before: .value(msg.value).gas(2300)
        //           ^ Return value deconstruction with skipping by omission.
        if (!success) revert();

        // High Level calling other contracts also allows sending to external payable funcs.
        // _target.payMe{value: 1 wei}();
        // Because the Test Interface/Contract has payable functions, the address to cast from must be paybable too
        ITest test = ITest(ptestAddress);
        test.payMe{value: 1 wei}();
    }
}

/**
 * Libraries have no state or balance and do not support inheritance.
 *
 * Embedded Library: If a smart contract is consuming a library which have only internal functions,
 * then the EVM simply embeds library into the contract. Instead of using delegate call to call a
 * function, it simply uses JUMP statement(normal method call). Same as free internal function are embedded.
 *
 * Linked Library : If a library contain public or external functions then library needs to be deployed.
 * The deployment of library will generate a unique address in the blockchain.
 * This address needs to be linked with the delegate-calling contract.
 * (CALLCODE instead of DELEGATECALL was used until Homestead, version 0.4.20 - msg.sender, value, etc - changed!)
 */
library UnsafeMath {


    /**
     * A library can be attached to a type inside a contract (only active within that contract:
     *   "using UnsafeMath for uint8;"
     * These functions will receive the object they are called on as their first parameter.
     *
     * External/Public function of linked libraries can only be called via DELEGATECALL.
     * This is an automatic protection to prevent killing libraries by calling a function
     * containing SELFDESTRUCT() which would brick contract using the library.
     */
    function sub(uint8 a, uint8 b) public pure returns (uint8) {
        // Before 0.8.0 a SafeMath library should have been used, now its save
        //  by default since it'll error when it wraps (checked mode).
        // Old wrapping behavior still available in unchecked mode:
        unchecked {
            a -= b; // Shorthand operators are supported.
        }
        return a;
    }

    function add(uint8 a, uint8 b) public pure returns (uint8) {
        unchecked {
            a += b;
        }
        return a;
    }

}
