<?php
/**
 * Smarty PHPunit tests of modifier.
 * This file should be saved in UTF-8 encoding for comment legibility.
 */

namespace UnitTests\TemplateSource\TagTests\PluginModifier;
use PHPUnit_Smarty;

class PluginModifierJsonEncodeCp1252Test extends PHPUnit_Smarty
{
	public function setUp(): void
	{
		$this->setUpSmarty(__DIR__);
		\Smarty\Smarty::$_CHARSET = 'cp1252';
	}

	public function tearDown(): void
	{
		\Smarty\Smarty::$_CHARSET = 'UTF-8';
	}

	/**
	 * @dataProvider dataForDefault
	 */
	public function testDefault($value, $expected)
	{
		$tpl = $this->smarty->createTemplate('string:{$v|json_encode}');
		$tpl->assign("v", $value);
		$this->assertEquals($expected, $this->smarty->fetch($tpl));
	}

	/**
	 * @dataProvider dataForDefault
	 */
	public function testDefaultAsFunction($value, $expected)
	{
		$tpl = $this->smarty->createTemplate('string:{json_encode($v)}');
		$tpl->assign("v", $value);
		$this->assertEquals($expected, $this->smarty->fetch($tpl));
	}

	public function dataForDefault() {
		$json_serializable_object = new class() implements \JsonSerializable {
			public function jsonSerialize(): mixed {
				return ["Schl\xC3\xBCssel" => "Stra\xC3\x9Fe"];	# UTF-8 ready for json_encode(); to prove that transcoding doesn't attempt to transcode this again
				#return ['Schlüssel' => 'Straße'];	# alternatively, this can be used, but then this file must always be saved in UTF-8 encoding or else the test will fail.
			}
		};
		return [
			["abc", '"abc"'],
			[["abc"], '["abc"]'],
			[["abc",["a"=>2]], '["abc",{"a":2}]'],
			[["\x80uro",["Schl\xFCssel"=>"Stra\xDFe"]], '["\u20acuro",{"Schl\u00fcssel":"Stra\u00dfe"}]'],	# x80 = € = euro, xFC = ü = uuml, xDF = ß = szlig
			[$json_serializable_object, '{"Schl\u00fcssel":"Stra\u00dfe"}'],
		];
	}

	/**
	 * @dataProvider dataForForceObject
	 */
	public function testForceObject($value, $expected)
	{
		$tpl = $this->smarty->createTemplate('string:{$v|json_encode:16}');
		$tpl->assign("v", $value);
		$this->assertEquals($expected, $this->smarty->fetch($tpl));
	}

	/**
	 * @dataProvider dataForForceObject
	 */
	public function testForceObjectAsFunction($value, $expected)
	{
		$tpl = $this->smarty->createTemplate('string:{json_encode($v,16)}');
		$tpl->assign("v", $value);
		$this->assertEquals($expected, $this->smarty->fetch($tpl));
	}

	public function dataForForceObject() {
		return [
			["abc", '"abc"'],
			[["abc"], '{"0":"abc"}'],
			[["abc",["a"=>2]], '{"0":"abc","1":{"a":2}}'],
			[["\x80uro"], '{"0":"\u20acuro"}'],
		];
	}

}
