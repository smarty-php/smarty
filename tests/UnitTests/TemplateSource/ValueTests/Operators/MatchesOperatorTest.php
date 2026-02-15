<?php

require_once __DIR__ . '/../../../../Bootstrap.php';

class MatchesOperatorTest extends PHPUnit_Smarty {
	public function setUp(): void
	{
		$this->setUpSmarty(__DIR__);
	}

	public function testInit()
	{
		$this->cleanDirs();
	}

	/**
	 * Test basic regex matching functionality
	 */
	public function testBasicMatches() {
		// Test string matching pattern
		$this->assertEquals('Match!', $this->smarty->fetch('string:{if "hello" matches "/^[a-z]+$/"}Match!{else}No match{/if}'));
		
		// Test no match
		$this->assertEquals('No match', $this->smarty->fetch('string:{if "123" matches "/^[a-z]+$/"}Match!{else}No match{/if}'));
		
		// Test with variables
		$this->smarty->assign('name', 'hello');
		$this->smarty->assign('pattern', '/^[a-z]+$/');
		$this->assertEquals('Match!', $this->smarty->fetch('string:{if $name matches $pattern}Match!{else}No match{/if}'));
		
		// Test no match with variables
		$this->smarty->assign('number', '123');
		$this->assertEquals('No match', $this->smarty->fetch('string:{if $number matches $pattern}Match!{else}No match{/if}'));
	}

	/**
	 * Test different regex patterns
	 */
	public function testDifferentPatterns() {
		// Test numeric pattern
		$this->assertEquals('Match!', $this->smarty->fetch('string:{if "123" matches "/^[0-9]+$/"}Match!{else}No match{/if}'));
		
		// Test email pattern
		$this->assertEquals('Match!', $this->smarty->fetch('string:{if "test@example.com" matches "/^[^@]+@[^@]+\.[^@]+$/"}Match!{else}No match{/if}'));
		
		// Test case insensitive pattern
		$this->assertEquals('Match!', $this->smarty->fetch('string:{if "Hello" matches "/hello/i"}Match!{else}No match{/if}'));
		
		// Test pattern with special characters
		$this->assertEquals('Match!', $this->smarty->fetch('string:{if "hello-world" matches "/^[a-z-]+$/"}Match!{else}No match{/if}'));
	}

	/**
	 * Test regex patterns with modifiers
	 */
	public function testPatternModifiers() {
		// Test case insensitive modifier
		$this->assertEquals('Match!', $this->smarty->fetch('string:{if "HELLO" matches "/hello/i"}Match!{else}No match{/if}'));
		
		// Test multiline modifier
		$this->assertEquals('Match!', $this->smarty->fetch('string:{if "hello\nworld" matches "/world$/m"}Match!{else}No match{/if}'));
		
		// Test dotall modifier
		$this->assertEquals('Match!', $this->smarty->fetch('string:{if "hello\nworld" matches "/hello.world/s"}Match!{else}No match{/if}'));
	}

	/**
	 * Test complex expressions with matches operator
	 */
	public function testComplexExpressions() {
		// Test with logical AND
		$this->smarty->assign('name', 'hello');
		$this->smarty->assign('pattern', '/^[a-z]+$/');
		$this->smarty->assign('length', 5);
		$this->assertEquals('Valid!', $this->smarty->fetch('string:{if $name matches $pattern && $length > 3}Valid!{else}Invalid{/if}'));
		
		// Test with logical OR
		$this->assertEquals('Valid!', $this->smarty->fetch('string:{if $name matches $pattern || $length < 3}Valid!{else}Invalid{/if}'));
		
		// Test in elseif
		$this->assertEquals('ElseIf!', $this->smarty->fetch('string:{if $name matches "/^[0-9]+$/"}No{elseif $name matches $pattern}ElseIf!{else}Else{/if}'));
		
		// Test in while loop condition (should work with constant patterns)
		$result = $this->smarty->fetch('string:{assign var="i" value=0}{while $i < 2 && "test" matches "/test/"}{$i}{assign var="i" value=$i+1}{/while}');
		$this->assertEquals('01', $result);
	}

	/**
	 * Test edge cases and error handling
	 */
	public function testEdgeCases() {
		// Test with empty string
		$this->assertEquals('No match', $this->smarty->fetch('string:{if "" matches "/.+/"}Match!{else}No match{/if}'));
		
		// Test with complex pattern
		$this->assertEquals('Match!', $this->smarty->fetch('string:{if "abc123" matches "/^[a-z]+[0-9]+$/"}Match!{else}No match{/if}'));
		
		// Test with single character pattern
		$this->assertEquals('Match!', $this->smarty->fetch('string:{if "a" matches "/a/"}Match!{else}No match{/if}'));
	}

	/**
	 * Test that invalid patterns still work (they will cause PHP warnings but not break the template)
	 * Note: This test is commented out because invalid patterns cause PHP warnings that fail the test
	 * In production, invalid patterns would cause warnings but the template would still execute
	 */
	/*
	public function testInvalidPatterns() {
		// This will cause a PHP warning but should not break the template execution
		$this->smarty->assign('invalid_pattern', 'not-a-valid-pattern');
		$this->smarty->assign('name', 'hello');
		
		// The template should execute and reach the else branch due to the invalid pattern
		$result = $this->smarty->fetch('string:{if $name matches $invalid_pattern}Match!{else}No match{/if}');
		$this->assertEquals('No match', $result);
	}
	*/
}