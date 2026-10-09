<?php declare(strict_types=1);

namespace Smarty\Extension\DefaultExtension;

use Smarty\Exception;

class RecursiveTranscoder {

	/**
	 * Determines if the given value is a possible candidate for transcoding.
	 *
	 * @param mixed $value
	 * @return bool
	 */
	public static function is_transcoding_candidate($value): bool {
		if (empty($value)) {
			return false;
		}
		return is_string($value) || is_array($value) || is_object($value);
	}


	/**
	 * Similar to mb_convert_encoding(), but operates recursively on keys and values of arrays, and on objects too.
	 * Objects implementing \JsonSerializable and unsupported types are returned unchanged.
	 * The following boolean options/hints are supported (all default to false):
	 *	- ignore_keys: do not transcode array keys
	 *	- ignore_objects: leave objects alone, i.e. do not convert them into associative arrays and transcode them
	 *	- ignore_JsonSerializable_objects: if transcoded result is meant as input for json_encode(), then set this to true.
	 *
	 * @param mixed $data
	 * @param string $to_encoding
	 * @param string $from_encoding of $data; defaults to \Smarty\Smarty::$_CHARSET
	 * @param array $options
	 * @return mixed
	 */
	public static function transcode($data, string $to_encoding, string $from_encoding = null, array $options = null) {
		if (!static::is_transcoding_candidate($data)) {
			return $data;
		}
		if (!$from_encoding) {
			$from_encoding = \Smarty\Smarty::$_CHARSET;
		}
		if (strcasecmp($to_encoding, $from_encoding) == 0) {
			return $data;
		}

		# most cases:
		if (is_string($data)) {
			return mb_convert_encoding($data, $to_encoding, $from_encoding);	# string|false
		}

		# convert object to array to be transcoded as array
		if (is_object($data)) {
			if (!empty($options['ignore_objects'])) {
				return $data;
			}
			if (is_a($data, \JsonSerializable::class)) {
				if (!empty($options['ignore_JsonSerializable_objects'])) {
					return $data;	# \JsonSerializable objects should be trusted to serialize themselves into data that can be consumed by json_encode() no matter what the application's default encoding is.
				}
			}
			$data = get_object_vars($data); # public properties as key => value pairs
		}

		if (!(is_array($data) && $data)) {
			return $data;	# any empty array or non-array type as a possible result of object conversion above
		}

		# $data is a filled array
		$must_transcode_keys = empty($options['ignore_keys']);
		$result = $must_transcode_keys ? [] : null;	# replacement for $data if keys are transcoded too (i.e. $must_transcode_keys)
		$this_func = __FUNCTION__;	# for recursion
		foreach ($data as $k => &$v) {
			if ($must_transcode_keys && is_string($k)) {
				$converted_k = mb_convert_encoding($k, $to_encoding, $from_encoding);	# string|false
				if ($converted_k === false) {	# this means mb_convert_encoding() failed which should've triggered a warning
					# One of three things can be done here:
					# 1. throw an Exception
					# 2. return false, indicating to caller that mb_convert_encoding() failed
					# 3. do nothing and use the original key
					#return false;
					throw Exception("Failed to encode array key \"$k\" from $from_encoding to $to_encoding");
				}
				else {
					$k = $converted_k;
				}
			}
			if (static::is_transcoding_candidate($v)) {
				# recurse
				$converted_v = static::$this_func($v, $to_encoding, $from_encoding, $options);
				if ($converted_v === false) {	# this means that $v is a string and that mb_convert_encoding() failed, which should've triggered a warning
					# One of four things can be done here:
					# 1. throw an Exception
					# 2. return false, indicating to caller that mb_convert_encoding() failed
					# 3. do nothing and use the original value
					# 4. replace the original value with false
					#return false;
					throw Exception('Failed to encode array value' . (is_string($v) ? " \"$k\"" : '')  . 'of type ' . gettype($v) . " from $from_encoding to $to_encoding");
				}
				else {
					$v = $converted_v;
					if ($must_transcode_keys) {
						$result[$k] = $v;
					}
				}
			}
			else {
				# $v may be false here, and in this case it is not an error (since no transcoding occurred since it's not a transcoding candidate)
				if ($must_transcode_keys) {
					$result[$k] = $v;
				}
			}
			unset($v);
		}
		return $must_transcode_keys ? $result : $data;
	}

}
