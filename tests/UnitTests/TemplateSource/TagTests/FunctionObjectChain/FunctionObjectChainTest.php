<?php
/**
 * Smarty PHPunit tests for object chain functionality after function calls
 * Tests the new feature: {$x = collect($data)->filter()->values()->toJson()}
 *
 * @author  Smarty Vibe
 */

/**
 * Helper class to simulate a chainable collection object
 */
class ChainableCollection
{
    private $data;

    public function __construct($data)
    {
        $this->data = $data;
    }

    public function filter()
    {
        $this->data = array_filter($this->data);
        return $this;
    }

    public function values()
    {
        $this->data = array_values($this->data);
        return $this;
    }

    public function toJson()
    {
        return json_encode($this->data);
    }

    public function toArray()
    {
        return $this->data;
    }

    public function count()
    {
        return count($this->data);
    }
}

/**
 * Modifier plugin that returns a chainable object (simulates collect())
 */
function smarty_modifier_collect($data)
{
    return new ChainableCollection($data);
}

/**
 * Modifier plugin that returns an object with methods
 */
function smarty_modifier_create_object($value = null)
{
    return new class {
        public function getName() {
            return 'TestObject';
        }

        public function getNext() {
            return new class {
                public function getValue() {
                    return 'ChainedValue';
                }
            };
        }
    };
}

/**
 * class for function object chain tests
 *
 * @preserveGlobalState    disabled
 *
 */
class FunctionObjectChainTest extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(__DIR__);

        // Register modifier plugins that return chainable objects
        // These are called like functions: {collect($data)}
        $this->smarty->registerPlugin(\Smarty\Smarty::PLUGIN_MODIFIER, 'collect', 'smarty_modifier_collect');
        $this->smarty->registerPlugin(\Smarty\Smarty::PLUGIN_MODIFIER, 'create_object', 'smarty_modifier_create_object');
    }

    public function testInit()
    {
        $this->cleanDirs();
    }

    /**
     * Test the NEW feature: function call followed by method chain
     * This is the core test for: {$x = collect($data)->filter()->values()->toJson()}
     */
    public function testFunctionCallWithMethodChain()
    {
        $data = [1, 2, 0, 3, null, 4, '', 5];
        $this->smarty->assign('data', $data);

        // Test the new syntax: function()->method()->method()->method()
        $result = $this->smarty->fetch('string:{collect($data)->filter()->values()->toJson()}');
        $this->assertEquals('[1,2,3,4,5]', $result);
    }

    /**
     * Test function call with method chain assigned to a variable
     * This tests: {$x = collect($data)->filter()->values()->toJson()}
     */
    public function testFunctionCallWithMethodChainAssignment()
    {
        $data = ['a', 'b', '', 'c', null, 'd'];
        $this->smarty->assign('data', $data);

        // Test assignment with chained methods
        $result = $this->smarty->fetch('string:{$result = collect($data)->filter()->values()->toJson()}{$result}');
        $this->assertEquals('["a","b","c","d"]', $result);
    }

    /**
     * Test function call with single method chain
     */
    public function testFunctionCallWithSingleMethod()
    {
        $data = [1, 2, 3];
        $this->smarty->assign('data', $data);

        $result = $this->smarty->fetch('string:{collect($data)->count()}');
        $this->assertEquals('3', $result);
    }

    /**
     * Test function call with nested method chains
     */
    public function testFunctionCallWithNestedChain()
    {
        $result = $this->smarty->fetch('string:{create_object("")->getNext()->getValue()}');
        $this->assertEquals('ChainedValue', $result);
    }

    /**
     * Test that old syntax still works (two-step process)
     */
    public function testOldSyntaxStillWorks()
    {
        $data = [1, 2, 0, 3];
        $this->smarty->assign('data', $data);

        // Old syntax: assign to variable first, then chain
        $result = $this->smarty->fetch('string:{$x = collect($data)}{$x->filter()->values()->toJson()}');
        $this->assertEquals('[1,2,3]', $result);
    }

    /**
     * Test object chain functionality using template file
     */
    public function testFunctionObjectChainFromTemplateFile()
    {
        $data = ['foo', '', 'bar', null, 'baz'];
        $this->smarty->assign('data', $data);

        $result = $this->smarty->fetch('test_function_chain.tpl');
        // Expected: JSON of filtered array and count
        $this->assertStringContainsString('["foo","bar","baz"]', $result);
        $this->assertStringContainsString('3', $result);
    }

    /**
     * Test complex chaining with multiple operations
     * This demonstrates the power of the new feature
     */
    public function testComplexChaining()
    {
        $data = [1, 2, 0, 3, '', 4, null, 5, false, 6];
        $this->smarty->assign('data', $data);

        // Complex chain: collect -> filter -> values -> toArray
        $result = $this->smarty->fetch('string:{$filtered = collect($data)->filter()->values()->toArray()}{$filtered|@json_encode}');
        $this->assertEquals('[1,2,3,4,5,6]', $result);
    }

    /**
     * Test that demonstrates the benefit: one line vs multiple lines
     */
    public function testNewSyntaxVsOldSyntax()
    {
        $data = [10, 20, 0, 30];
        $this->smarty->assign('data', $data);

        // NEW syntax (single line)
        $new = $this->smarty->fetch('string:{collect($data)->filter()->count()}');

        // OLD syntax (multiple steps)
        $old = $this->smarty->fetch('string:{$temp = collect($data)}{$temp = $temp->filter()}{$temp->count()}');

        // Both should produce the same result
        $this->assertEquals('3', $new);
        $this->assertEquals($new, $old);
    }
}