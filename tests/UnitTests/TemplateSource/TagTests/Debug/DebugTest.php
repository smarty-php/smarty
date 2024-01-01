<?php

namespace UnitTests\TemplateSource\TagTests\Debug;
use PHPUnit_Smarty;
use Smarty\Smarty;

/**
 * Smarty PHPunit tests of {debug} tag
 */
class PluginModifierStripTagsTest extends PHPUnit_Smarty {

	public function setUp(): void {
		$this->setUpSmarty(__DIR__);
	}
	public function testDefault() {
		$tpl = $this->smarty->createTemplate('eval:{debug}');
		$output = $this->smarty->fetch($tpl);
		$this->assertStringContainsString("<script", $output);
		$this->assertStringContainsString("</script>", $output);
	}

}
