<?php

namespace Smarty\CodeFrame;

abstract class Cached extends Base {

	protected function isCache(): bool {
		return true;
	}
}