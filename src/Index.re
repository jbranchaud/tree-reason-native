type path = {
  full_path: string,
  name: string,
};

type file_item =
  | File(path)
  | Directory(path, array(file_item));

let print_dir_with_pipes = (dirname, level) =>
  switch (level) {
  | 0 => print_endline(dirname)
  | 1 => print_endline("└── " ++ dirname)
  | num =>
    String.make((num - 1) * 4, ' ')
    ++ "└── "
    ++ dirname
    |> print_endline
  };

let rec print_directory_at_level = (file_item, level) =>
  switch (file_item) {
  | File({full_path: _, name: filename}) =>
    print_dir_with_pipes(filename, level)
  | Directory({full_path: _, name: dirname}, [||]) =>
    print_dir_with_pipes(dirname, level)
  | Directory({full_path: _, name: dirname}, contents) =>
    print_dir_with_pipes(dirname, level);
    let print_sub_dir = file_item =>
      print_directory_at_level(file_item, level + 1);
    Array.iter(print_sub_dir, contents);
  };

let print_directory = file_item => print_directory_at_level(file_item, 0);

let rec walk_directory_tree = path : file_item => {
  let current_dir_handle = Unix.opendir(path.full_path);

  let break = ref(false);

  let contents = ref([||]);
  while (! break^) {
    switch (Unix.readdir(current_dir_handle)) {
    | exception End_of_file => break := true
    | item =>
      let item_stats: Unix.stats = Unix.stat(path.full_path ++ "/" ++ item);
      switch (item_stats.st_kind) {
      | Unix.S_REG =>
        let full_path = path.full_path ++ "/" ++ item;
        contents :=
          Array.append(contents^, [|File({full_path, name: item})|]);
        ignore();
      | Unix.S_DIR =>
        switch (item) {
        | "."
        | ".." => ignore()
        | sub_dir_name =>
          let full_path = path.full_path ++ "/" ++ sub_dir_name;
          let sub_dir = walk_directory_tree({full_path, name: sub_dir_name});
          contents := Array.append(contents^, [|sub_dir|]);
          ignore();
        }
      | _ => ignore()
      };
    };
  };

  Unix.closedir(current_dir_handle);

  Directory(path, contents^);
};

let run = () => {
  let path_parts = Str.split(Str.regexp("/"), Unix.getcwd());
  let basename = List.nth(path_parts, List.length(path_parts) - 1);
  walk_directory_tree({full_path: Unix.getcwd(), name: basename})
  |> print_directory;
};

/* example:
   lib
   └── bs
       └── bytecode
           ├── basic-ls
           ├── build.ninja
           ├── src
           │   ├── Index.cmi
           │   ├── Index.cmo
   */

run();