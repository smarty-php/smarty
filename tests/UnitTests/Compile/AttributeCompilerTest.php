<?php

use Smarty\Compile\AttributeCompiler;
use Smarty\Compiler\Template;

class AttributeCompilerTest extends PHPUnit\Framework\TestCase
{
	/**
	 * The template compiler.
	 */
	private $template_compiler;

	/**
	 * The attributes
	 */
	private $attributes = [];

	/**
	 * @inheritDoc
	 * Set up attribute compiler class
	 */
	protected function setUp(): void
	{
		$this->template_compiler = $this->createMock(Template::class);

		// reset attributes to empty arrays
		$this->attributes = [
			'required_attributes' => [],
			'optional_attributes' => [],
			'shorttag_order' => [],
			'option_flags' => [],
		];
	}

	/**
	 * Create the attribute compiler for testing.
	 */
	private function createAttributeCompiler() {
		return new AttributeCompiler(
			$this->attributes['required_attributes'],
			$this->attributes['optional_attributes'],
			$this->attributes['shorttag_order'],
			$this->attributes['option_flags']
		);
	}

	/**
	 * Tests shorthand attribute compiling.
	 */
	public function testAttributeCompiler(): void
	{
		$this->attributes['shorttag_order'] = ['shorttag'];
		$this->attributes['required_attributes'] = ['required'];
		$this->attributes['option_flags'] = ['option', 'option_two'];

		$payload = [
			0 => 'shorttag value',
			1 => [
				'required' => 'required_value'
			],
			2 => 'option',
		];

		$this->assertEquals(
			$this->createAttributeCompiler()
				->getAttributes($this->template_compiler, $payload),
			[
				'shorttag' => 'shorttag value',
				'required' => 'required_value',
				'option' => true,
				'option_two' => false,
			]
		);
	}

	/**
	 * Tests normal optional attribute compiling.
	 */
	public function testAttributeCompilerOptionalArguments(): void
	{
		$this->attributes['optional_attributes'] = ['optional'];

		$payload = [
			0 => [
				'optional' => 'optional value'
			],
		];

		$this->assertEquals(
			$this->createAttributeCompiler()
				->getAttributes($this->template_compiler, $payload),
			[
				'optional' => 'optional value',
			]
		);

		$this->assertEquals(
			$this->createAttributeCompiler()
				->getAttributes($this->template_compiler, []),
			[]
		);
	}

	/** 
	 * Tests any attribute compiling.
	 */
	public function testAttributeCompilerAnyOptionalArguments(): void
	{
		$this->attributes['optional_attributes'] = ['_any'];

		$payload = [
			0 => [
				'optional' => 'optional value'
			],
			1 => [
				'optional_two' => 'optional value two'
			],
		];

		$this->assertEquals(
			$this->createAttributeCompiler()
				->getAttributes($this->template_compiler, $payload),
			[
				'optional' => 'optional value',
				'optional_two' => 'optional value two',
			]
		);
	}

	/**
	 * Test if the attribute compiler tries to throw a too many shorthand attributes error.
	 */
	public function testAttributeCompilerTooManyShorthands(): void
	{
		$payload = [
			0 => 'option one',
		];

		$this->template_compiler
			->expects(self::once())
			->method('trigger_template_error')
			->with('too many shorthand attributes', null, true);

		$this->createAttributeCompiler()
			->getAttributes($this->template_compiler, $payload);
	}

	/**
	 * Test if the attribute compiler tries to throw a missing required attribute error.
	 */
	public function testAttributeCompilerWithMissingRequiredAttributes(): void
	{
		$this->attributes['required_attributes'] = ['required'];

		$this->template_compiler
			->expects(self::once())
			->method('trigger_template_error')
			->with('missing \'required\' attribute', null, true);

		$this->createAttributeCompiler()
			->getAttributes($this->template_compiler, []);
	}

	/**
	 * Test if the attribute compiler tries to throw a illegal value template error.
	 */
	public function testAttributeCompilerWithInvalidOptionAttribute(): void
	{
		$this->attributes['option_flags'] = ['option'];

		$this->template_compiler
			->expects(self::once())
			->method('trigger_template_error')
			->with('illegal value \'\'foo\'\' for options flag \'option\'', null, true);

		$this->createAttributeCompiler()
			->getAttributes($this->template_compiler, [0 => ['option' => 'foo']]);
	}

	/**
	 * Test if the attribute compiler tries to throw an unexpected attribute error.
	 */
	public function testAttributeCompilerWithInvalidUnexpectedAttribute(): void
	{
		$this->template_compiler
			->expects(self::once())
			->method('trigger_template_error')
			->with('unexpected \'unexpected\' attribute', null, true);

		$this->createAttributeCompiler()
			->getAttributes($this->template_compiler, [0 => ['unexpected' => 'bar']]);
	}
}
