//
// Copyright 2020 The Project Oak Authors
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//

use crate::Handle;

/// Encapsulates a message consisting of opaque data bytes and a vector of channels.
/// The data bytes should not contain any pointers or handles.
#[derive(Debug)]
pub struct Message {
    pub data: Vec<u8>,
    pub channels: Vec<Handle>,
}