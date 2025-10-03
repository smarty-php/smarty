<?php

use Smarty\Compile\FunctionCallCompiler;
use Smarty\Compiler\Template;
use Smarty\FunctionHandler\AttributeFunctionHandlerInterface;
use Smarty\Smarty;

class FunctionCallCompilerTest extends PHPUnit\Framework\TestCase
{
	/**
	 * @inheritDoc
	 * Set up attribute compiler class
	 */
	protected function setUp(): void
	{
		$this->smarty = $this->createMock(Smarty::class);
		$this->template_compiler = $this->createMock(Template::class);
		$this->template_compiler
			->expects(self::once())
			->method('getSmarty')
			->willReturn($this->smarty);
	}

	public function testAttributeFunctionHandlerInterface(): void
	{
		$attribute_function_handler = $this->createMock(AttributeFunctionHandlerInterface::class);

		$attribute_function_handler
			->expects(self::once())
			->method('getSupportedAttributes')
			->willReturn([
				'required_attributes' => ['required'],
				'optional_attributes' => ['optional'],
				'shorttag_order' => ['short'],
				'option_flags' => ['option'],
			]);

		$args = [
			0 => 'short',
			1 => 'option',
			2 => [
				'optional' => 'optional',
			],
			3 => [
				'required' => 'required',
			],
		];

		$this->smarty
			->expects(self::once())
			->method('getFunctionHandler')
			->with('method')
			->willReturn($attribute_function_handler);

		$function_call_compiler = new FunctionCallCompiler();

		$this->assertEquals(
			$function_call_compiler->compile($args, $this->template_compiler, [], null, 'method'),
			'$_smarty_tpl->getSmarty()->getFunctionHandler(\'method\')->handle(array(\'short\'=>short,\'option\'=>1,\'optional\'=>optional,\'required\'=>required), $_smarty_tpl)'
		);
	}
}
