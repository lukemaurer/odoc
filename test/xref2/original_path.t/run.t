  $ cat b.mli
  module X : sig type t end
  
  module type X1 = module type of X
  
  module type X2 = module type of A
  
  module type X3 = module type of A.X
  
  module Y : functor (A : sig type t end) -> sig type t end
  
  module type Foo = sig
      module X : sig type t end
  
      module Y : functor (A : sig type t end) -> sig module Z = X end
  
      module type Z = module type of Y
  end
  
  module type X4 = module type of Y
  
  module SubX : sig
      type t
      type u
  end
  
  module type X5 = Foo with module X := SubX
  
  module type X6 = module type of A.X
  

  $ ocamlc -c -bin-annot a.mli
  $ ocamlc -c -bin-annot b.mli
  $ odoc compile a.cmti
  $ odoc compile -I . b.cmti
  $ odoc link -I . a.odoc
  $ odoc link -I . b.odoc
  $ odoc_print b.odocl -r X5
  {
    "id": { "`ModuleType": [ { "`Root": [ "None", "B" ] }, "X5" ] },
    "locs": "None",
    "doc": [],
    "canonical": "None",
    "expr": {
      "Some": {
        "With": {
          "w_substitutions": [
            {
              "ModuleSubst": [
                {
                  "`Resolved": {
                    "`Module": [
                      {
                        "`Root": {
                          "`ModuleType": {
                            "`Identifier": {
                              "`ModuleType": [
                                { "`Root": [ "None", "B" ] }, "Foo"
                              ]
                            }
                          }
                        }
                      },
                      "X"
                    ]
                  }
                },
                {
                  "`Resolved": {
                    "`Identifier": {
                      "`Module": [ { "`Root": [ "None", "B" ] }, "SubX" ]
                    }
                  }
                }
              ]
            }
          ],
          "w_expansion": {
            "Some": {
              "Signature": {
                "items": [
                  {
                    "Module": [
                      "Ordinary",
                      {
                        "id": {
                          "`Module": [
                            {
                              "`ModuleType": [
                                { "`Root": [ "None", "B" ] }, "X5"
                              ]
                            },
                            "Y"
                          ]
                        },
                        "locs": "None",
                        "doc": [],
                        "type_": {
                          "ModuleType": {
                            "Functor": [
                              {
                                "Named": {
                                  "id": {
                                    "`Parameter": [
                                      {
                                        "`Module": [
                                          {
                                            "`ModuleType": [
                                              { "`Root": [ "None", "B" ] },
                                              "X5"
                                            ]
                                          },
                                          "Y"
                                        ]
                                      },
                                      "A"
                                    ]
                                  },
                                  "expr": {
                                    "Signature": {
                                      "items": [
                                        {
                                          "Type": [
                                            "Ordinary",
                                            {
                                              "id": {
                                                "`Type": [
                                                  {
                                                    "`Parameter": [
                                                      {
                                                        "`Module": [
                                                          {
                                                            "`ModuleType": [
                                                              {
                                                                "`Root": [
                                                                  "None", "B"
                                                                ]
                                                              },
                                                              "X5"
                                                            ]
                                                          },
                                                          "Y"
                                                        ]
                                                      },
                                                      "A"
                                                    ]
                                                  },
                                                  "t"
                                                ]
                                              },
                                              "locs": "None",
                                              "doc": [],
                                              "equation": {
                                                "params": [],
                                                "private_": "false",
                                                "manifest": "None",
                                                "constraints": []
                                              },
                                              "representation": "None"
                                            }
                                          ]
                                        }
                                      ],
                                      "compiled": "true",
                                      "doc": []
                                    }
                                  }
                                }
                              },
                              {
                                "Signature": {
                                  "items": [
                                    {
                                      "Module": [
                                        "Ordinary",
                                        {
                                          "id": {
                                            "`Module": [
                                              {
                                                "`Result": {
                                                  "`Module": [
                                                    {
                                                      "`ModuleType": [
                                                        {
                                                          "`Root": [
                                                            "None", "B"
                                                          ]
                                                        },
                                                        "X5"
                                                      ]
                                                    },
                                                    "Y"
                                                  ]
                                                }
                                              },
                                              "Z"
                                            ]
                                          },
                                          "locs": "None",
                                          "doc": [],
                                          "type_": {
                                            "Alias": [
                                              {
                                                "`Resolved": {
                                                  "`Identifier": {
                                                    "`Module": [
                                                      {
                                                        "`Root": [
                                                          "None", "B"
                                                        ]
                                                      },
                                                      "SubX"
                                                    ]
                                                  }
                                                }
                                              },
                                              "None"
                                            ]
                                          },
                                          "canonical": "None",
                                          "hidden": "false"
                                        }
                                      ]
                                    }
                                  ],
                                  "compiled": "true",
                                  "doc": []
                                }
                              }
                            ]
                          }
                        },
                        "canonical": "None",
                        "hidden": "false"
                      }
                    ]
                  },
                  {
                    "ModuleType": {
                      "id": {
                        "`ModuleType": [
                          {
                            "`ModuleType": [
                              { "`Root": [ "None", "B" ] }, "X5"
                            ]
                          },
                          "Z"
                        ]
                      },
                      "locs": "None",
                      "doc": [],
                      "canonical": "None",
                      "expr": {
                        "Some": {
                          "TypeOf": {
                            "t_desc": {
                              "ModPath": {
                                "`Resolved": {
                                  "`Identifier": {
                                    "`Module": [
                                      {
                                        "`ModuleType": [
                                          { "`Root": [ "None", "B" ] }, "X5"
                                        ]
                                      },
                                      "Y"
                                    ]
                                  }
                                }
                              }
                            },
                            "t_expansion": {
                              "Some": {
                                "Functor": [
                                  {
                                    "Named": {
                                      "id": {
                                        "`Parameter": [
                                          {
                                            "`ModuleType": [
                                              {
                                                "`ModuleType": [
                                                  { "`Root": [ "None", "B" ] },
                                                  "X5"
                                                ]
                                              },
                                              "Z"
                                            ]
                                          },
                                          "A"
                                        ]
                                      },
                                      "expr": {
                                        "Signature": {
                                          "items": [
                                            {
                                              "Type": [
                                                "Ordinary",
                                                {
                                                  "id": {
                                                    "`Type": [
                                                      {
                                                        "`Parameter": [
                                                          {
                                                            "`ModuleType": [
                                                              {
                                                                "`ModuleType": [
                                                                  {
                                                                    "`Root": [
                                                                      "None",
                                                                      "B"
                                                                    ]
                                                                  },
                                                                  "X5"
                                                                ]
                                                              },
                                                              "Z"
                                                            ]
                                                          },
                                                          "A"
                                                        ]
                                                      },
                                                      "t"
                                                    ]
                                                  },
                                                  "locs": "None",
                                                  "doc": [],
                                                  "equation": {
                                                    "params": [],
                                                    "private_": "false",
                                                    "manifest": "None",
                                                    "constraints": []
                                                  },
                                                  "representation": "None"
                                                }
                                              ]
                                            }
                                          ],
                                          "compiled": "true",
                                          "doc": []
                                        }
                                      }
                                    }
                                  },
                                  {
                                    "Signature": {
                                      "items": [
                                        {
                                          "Module": [
                                            "Ordinary",
                                            {
                                              "id": {
                                                "`Module": [
                                                  {
                                                    "`Result": {
                                                      "`ModuleType": [
                                                        {
                                                          "`ModuleType": [
                                                            {
                                                              "`Root": [
                                                                "None", "B"
                                                              ]
                                                            },
                                                            "X5"
                                                          ]
                                                        },
                                                        "Z"
                                                      ]
                                                    }
                                                  },
                                                  "Z"
                                                ]
                                              },
                                              "locs": "None",
                                              "doc": [],
                                              "type_": {
                                                "Alias": [
                                                  {
                                                    "`Resolved": {
                                                      "`Identifier": {
                                                        "`Module": [
                                                          {
                                                            "`Root": [
                                                              "None", "B"
                                                            ]
                                                          },
                                                          "SubX"
                                                        ]
                                                      }
                                                    }
                                                  },
                                                  "None"
                                                ]
                                              },
                                              "canonical": "None",
                                              "hidden": "false"
                                            }
                                          ]
                                        }
                                      ],
                                      "compiled": "true",
                                      "doc": []
                                    }
                                  }
                                ]
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                ],
                "compiled": "true",
                "doc": []
              }
            }
          },
          "w_expr": {
            "Path": {
              "`Resolved": {
                "`Identifier": {
                  "`ModuleType": [ { "`Root": [ "None", "B" ] }, "Foo" ]
                }
              }
            }
          }
        }
      }
    }
  }
 
